// Azimuth Telemetry System - Serialization and Deserialization Comprehensive Tests
// This file contains comprehensive test cases for data serialization and deserialization

// Test 1: JSON Serialization and Deserialization
test "json serialization and deserialization" {
  // Test JSON serializer
  let json_serializer = JsonSerializer::new()
  
  // Test basic types serialization
  let json_string = JsonSerializer::serialize(json_serializer, "hello world")
  assert_eq(json_string, "\"hello world\"")
  
  let json_number = JsonSerializer::serialize(json_serializer, 42)
  assert_eq(json_number, "42")
  
  let json_float = JsonSerializer::serialize(json_serializer, 3.14)
  assert_eq(json_float, "3.14")
  
  let json_boolean = JsonSerializer::serialize(json_serializer, true)
  assert_eq(json_boolean, "true")
  
  // Test array serialization
  let json_array = JsonSerializer::serialize(json_serializer, [1, 2, 3, 4, 5])
  assert_eq(json_array, "[1,2,3,4,5]")
  
  // Test object serialization
  let test_object = TestObject::new("test", 123, true)
  let json_object = JsonSerializer::serialize(json_serializer, test_object)
  assert_true(json_object.contains("\"name\":\"test\""))
  assert_true(json_object.contains("\"value\":123"))
  assert_true(json_object.contains("\"active\":true"))
  
  // Test nested object serialization
  let nested_object = NestedObject::new(
    "parent",
    TestObject::new("child", 456, false)
  )
  let json_nested = JsonSerializer::serialize(json_serializer, nested_object)
  assert_true(json_nested.contains("\"name\":\"parent\""))
  assert_true(json_nested.contains("\"child\""))
  assert_true(json_nested.contains("\"name\":\"child\""))
  
  // Test deserialization
  let deserialized_string = JsonSerializer::deserialize[String](json_serializer, "\"hello world\"")
  match deserialized_string {
    Ok(value) => assert_eq(value, "hello world")
    Error(_) => assert_true(false)
  }
  
  let deserialized_number = JsonSerializer::deserialize[Int](json_serializer, "42")
  match deserialized_number {
    Ok(value) => assert_eq(value, 42)
    Error(_) => assert_true(false)
  }
  
  let deserialized_array = JsonSerializer::deserialize[Array[Int]](json_serializer, "[1,2,3,4,5]")
  match deserialized_array {
    Ok(array) => assert_eq(array, [1, 2, 3, 4, 5])
    Error(_) => assert_true(false)
  }
  
  let deserialized_object = JsonSerializer::deserialize[TestObject](json_serializer, json_object)
  match deserialized_object {
    Ok(obj) => {
      assert_eq(TestObject::get_name(obj), "test")
      assert_eq(TestObject::get_value(obj), 123)
      assert_eq(TestObject::is_active(obj), true)
    }
    Error(_) => assert_true(false)
  }
  
  // Test error handling
  let invalid_json = "{invalid json}"
  let error_result = JsonSerializer::deserialize[TestObject](json_serializer, invalid_json)
  match error_result {
    Ok(_) => assert_true(false)
    Error(e) => assert_true(e.contains("Invalid JSON"))
  }
  
  // Test JSON pretty printing
  let pretty_json = JsonSerializer::serialize_pretty(json_serializer, test_object)
  assert_true(pretty_json.contains("\n")) // Should contain newlines
  assert_true(pretty_json.contains("  ")) // Should contain indentation
}

// Test 2: Binary Serialization and Deserialization
test "binary serialization and deserialization" {
  // Test binary serializer
  let binary_serializer = BinarySerializer::new()
  
  // Test basic types serialization
  let binary_int = BinarySerializer::serialize(binary_serializer, 42)
  assert_eq(binary_int.length(), 4) // 4 bytes for int
  
  let binary_float = BinarySerializer::serialize(binary_serializer, 3.14)
  assert_eq(binary_float.length(), 8) // 8 bytes for float
  
  let binary_string = BinarySerializer::serialize(binary_serializer, "hello")
  assert_true(binary_string.length() > 5) // Length + string data
  
  // Test complex object serialization
  let complex_object = ComplexObject::new(
    "complex",
    [1, 2, 3],
    [("key1", "value1"), ("key2", "value2")]
  )
  let binary_complex = BinarySerializer::serialize(binary_serializer, complex_object)
  assert_true(binary_complex.length() > 0)
  
  // Test deserialization
  let deserialized_int = BinarySerializer::deserialize[Int](binary_serializer, binary_int)
  match deserialized_int {
    Ok(value) => assert_eq(value, 42)
    Error(_) => assert_true(false)
  }
  
  let deserialized_float = BinarySerializer::deserialize[Float](binary_serializer, binary_float)
  match deserialized_float {
    Ok(value) => assert_true(value - 3.14 < 0.001) // Allow for floating point precision
    Error(_) => assert_true(false)
  }
  
  let deserialized_string = BinarySerializer::deserialize[String](binary_serializer, binary_string)
  match deserialized_string {
    Ok(value) => assert_eq(value, "hello")
    Error(_) => assert_true(false)
  }
  
  let deserialized_complex = BinarySerializer::deserialize[ComplexObject](binary_serializer, binary_complex)
  match deserialized_complex {
    Ok(obj) => {
      assert_eq(ComplexObject::get_name(obj), "complex")
      assert_eq(ComplexObject::get_array(obj), [1, 2, 3])
      assert_eq(ComplexObject::get_map(obj).get("key1"), Some("value1"))
    }
    Error(_) => assert_true(false)
  }
  
  // Test binary format validation
  let format_validator = BinaryFormatValidator::new()
  let is_valid = BinaryFormatValidator::validate(format_validator, binary_complex)
  assert_true(is_valid)
  
  // Test binary compression
  let compressed_data = BinarySerializer::compress(binary_serializer, binary_complex)
  assert_true(compressed_data.length() < binary_complex.length()) // Should be smaller after compression
  
  let decompressed_data = BinarySerializer::decompress(binary_serializer, compressed_data)
  assert_eq(decompressed_data, binary_complex) // Should match original after decompression
}

// Test 3: Protocol Buffers Serialization
test "protocol buffers serialization" {
  // Test Protocol Buffers serializer
  let protobuf_serializer = ProtobufSerializer::new()
  
  // Test message definition and serialization
  let telemetry_message = TelemetryMessage::new(
    "sensor-1",
    1234567890,
    [("temperature", 25.5), ("humidity", 60.0)],
    ["location-1", "location-2"]
  )
  
  let protobuf_data = ProtobufSerializer::serialize(protobuf_serializer, telemetry_message)
  assert_true(protobuf_data.length() > 0)
  
  // Test deserialization
  let deserialized_message = ProtobufSerializer::deserialize[TelemetryMessage](protobuf_serializer, protobuf_data)
  match deserialized_message {
    Ok(msg) => {
      assert_eq(TelemetryMessage::get_sensor_id(msg), "sensor-1")
      assert_eq(TelemetryMessage::get_timestamp(msg), 1234567890)
      assert_eq(TelemetryMessage::get_metrics(msg).get("temperature"), Some(25.5))
      assert_eq(TelemetryMessage::get_tags(msg), ["location-1", "location-2"])
    }
    Error(_) => assert_true(false)
  }
  
  // Test nested protobuf messages
  let batch_message = BatchTelemetryMessage::new(
    "batch-1",
    [telemetry_message, telemetry_message],
    1234567890
  )
  
  let batch_protobuf = ProtobufSerializer::serialize(protobuf_serializer, batch_message)
  assert_true(batch_protobuf.length() > protobuf_data.length()) // Should be larger than single message
  
  let deserialized_batch = ProtobufSerializer::deserialize[BatchTelemetryMessage](protobuf_serializer, batch_protobuf)
  match deserialized_batch {
    Ok(batch) => {
      assert_eq(BatchTelemetryMessage::get_batch_id(batch), "batch-1")
      assert_eq(BatchTelemetryMessage::get_messages(batch).length(), 2)
    }
    Error(_) => assert_true(false)
  }
  
  // Test protobuf schema evolution
  // Simulate adding a new field to the schema
  let enhanced_message = EnhancedTelemetryMessage::new(
    "sensor-1",
    1234567890,
    [("temperature", 25.5)],
    ["location-1"],
    "device-type-1" // New field
  )
  
  let enhanced_protobuf = ProtobufSerializer::serialize(protobuf_serializer, enhanced_message)
  
  // Deserialize with old schema should still work (backward compatibility)
  let backward_compatible = ProtobufSerializer::deserialize[TelemetryMessage](protobuf_serializer, enhanced_protobuf)
  match backward_compatible {
    Ok(msg) => assert_eq(TelemetryMessage::get_sensor_id(msg), "sensor-1")
    Error(_) => assert_true(false)
  }
  
  // Test protobuf field validation
  let validator = ProtobufValidator::new()
  let validation_result = ProtobufValidator::validate(validator, telemetry_message)
  assert_true(validation_result.is_valid)
  
  // Test protobuf size optimization
  let size_analyzer = ProtobufSizeAnalyzer::new()
  let size_analysis = ProtobufSizeAnalyzer::analyze(size_analyzer, telemetry_message)
  assert_true(ProtobufSizeAnalysis::get_total_size(size_analysis) > 0)
  assert_true(ProtobufSizeAnalysis::get_field_sizes(size_analysis).length() > 0)
}

// Test 4: XML Serialization and Deserialization
test "xml serialization and deserialization" {
  // Test XML serializer
  let xml_serializer = XmlSerializer::new()
  
  // Test basic XML serialization
  let telemetry_data = TelemetryData::new(
    "sensor-1",
    1234567890,
    [("temperature", 25.5), ("humidity", 60.0)]
  )
  
  let xml_data = XmlSerializer::serialize(xml_serializer, telemetry_data)
  assert_true(xml_data.contains("<TelemetryData>"))
  assert_true(xml_data.contains("<sensorId>sensor-1</sensorId>"))
  assert_true(xml_data.contains("<timestamp>1234567890</timestamp>"))
  assert_true(xml_data.contains("<metrics>"))
  assert_true(xml_data.contains("<temperature>25.5</temperature>"))
  assert_true(xml_data.contains("</TelemetryData>"))
  
  // Test XML deserialization
  let deserialized_data = XmlSerializer::deserialize[TelemetryData](xml_serializer, xml_data)
  match deserialized_data {
    Ok(data) => {
      assert_eq(TelemetryData::get_sensor_id(data), "sensor-1")
      assert_eq(TelemetryData::get_timestamp(data), 1234567890)
      assert_eq(TelemetryData::get_metrics(data).get("temperature"), Some(25.5))
    }
    Error(_) => assert_true(false)
  }
  
  // Test XML with attributes
  let telemetry_with_attributes = TelemetryWithAttributes::new(
    "sensor-1",
    1234567890,
    [("temperature", 25.5)],
    [("unit", "celsius"), ("precision", "0.1")]
  )
  
  let xml_with_attributes = XmlSerializer::serialize(xml_serializer, telemetry_with_attributes)
  assert_true(xml_with_attributes.contains("unit=\"celsius\""))
  assert_true(xml_with_attributes.contains("precision=\"0.1\""))
  
  // Test XML namespace handling
  let namespaced_telemetry = NamespacedTelemetry::new(
    "sensor-1",
    1234567890,
    [("temperature", 25.5)]
  )
  
  let xml_with_namespace = XmlSerializer::serialize_with_namespace(
    xml_serializer,
    namespaced_telemetry,
    "http://example.com/telemetry",
    "tel"
  )
  
  assert_true(xml_with_namespace.contains("xmlns:tel=\"http://example.com/telemetry\""))
  assert_true(xml_with_namespace.contains("<tel:NamespacedTelemetry>"))
  
  // Test XML pretty printing
  let pretty_xml = XmlSerializer::serialize_pretty(xml_serializer, telemetry_data)
  assert_true(pretty_xml.contains("\n")) // Should contain newlines
  assert_true(pretty_xml.contains("  ")) // Should contain indentation
  
  // Test XML validation against schema
  let xml_validator = XmlValidator::new()
  let schema = XmlSchema::load("telemetry_schema.xsd") // Load schema file
  
  let validation_result = XmlValidator::validate(xml_validator, xml_data, schema)
  match validation_result {
    Ok(_) => assert_true(true) // Validation passed
    Error(e) => assert_true(false) // Validation failed
  }
  
  // Test XPath queries
  let xpath_processor = XPathProcessor::new()
  let temperature_value = XPathProcessor::query(xpath_processor, xml_data, "//temperature/text()")
  match temperature_value {
    Some(value) => assert_eq(value, "25.5")
    None => assert_true(false)
  }
  
  let sensor_id_value = XPathProcessor::query(xpath_processor, xml_data, "//sensorId/text()")
  match sensor_id_value {
    Some(value) => assert_eq(value, "sensor-1")
    None => assert_true(false)
  }
}

// Test 5: Custom Serialization Formats
test "custom serialization formats" {
  // Test CSV serialization
  let csv_serializer = CsvSerializer::new()
  
  let telemetry_records = [
    TelemetryRecord::new("sensor-1", 1234567890, 25.5, "location-1"),
    TelemetryRecord::new("sensor-2", 1234567891, 26.0, "location-2"),
    TelemetryRecord::new("sensor-3", 1234567892, 24.8, "location-3")
  ]
  
  let csv_data = CsvSerializer::serialize(csv_serializer, telemetry_records)
  assert_true(csv_data.contains("sensor-1,1234567890,25.5,location-1"))
  assert_true(csv_data.contains("sensor-2,1234567891,26.0,location-2"))
  assert_true(csv_data.contains("sensor-3,1234567892,24.8,location-3"))
  
  // Test CSV deserialization
  let deserialized_records = CsvSerializer::deserialize[Array[TelemetryRecord]](csv_serializer, csv_data)
  match deserialized_records {
    Ok(records) => {
      assert_eq(records.length(), 3)
      assert_eq(TelemetryRecord::get_sensor_id(records[0]), "sensor-1")
      assert_eq(TelemetryRecord::get_temperature(records[0]), 25.5)
    }
    Error(_) => assert_true(false)
  }
  
  // Test YAML serialization
  let yaml_serializer = YamlSerializer::new()
  
  let telemetry_config = TelemetryConfig::new(
    "config-1",
    ["sensor-1", "sensor-2"],
    [("interval", "60"), ("retry_count", "3")]
  )
  
  let yaml_data = YamlSerializer::serialize(yaml_serializer, telemetry_config)
  assert_true(yaml_data.contains("configId: config-1"))
  assert_true(yaml_data.contains("sensors:"))
  assert_true(yaml_data.contains("- sensor-1"))
  assert_true(yaml_data.contains("- sensor-2"))
  assert_true(yaml_data.contains("settings:"))
  assert_true(yaml_data.contains("interval: 60"))
  
  // Test YAML deserialization
  let deserialized_config = YamlSerializer::deserialize[TelemetryConfig](yaml_serializer, yaml_data)
  match deserialized_config {
    Ok(config) => {
      assert_eq(TelemetryConfig::get_config_id(config), "config-1")
      assert_eq(TelemetryConfig::get_sensors(config), ["sensor-1", "sensor-2"])
      assert_eq(TelemetryConfig::get_settings(config).get("interval"), Some("60"))
    }
    Error(_) => assert_true(false)
  }
  
  // Test TOML serialization
  let toml_serializer = TomlSerializer::new()
  
  let toml_data = TomlSerializer::serialize(toml_serializer, telemetry_config)
  assert_true(toml_data.contains("configId = \"config-1\""))
  assert_true(toml_data.contains("sensors = [\"sensor-1\", \"sensor-2\"]"))
  assert_true(toml_data.contains("[settings]"))
  assert_true(toml_data.contains("interval = \"60\""))
  
  // Test TOML deserialization
  let deserialized_toml = TomlSerializer::deserialize[TelemetryConfig](toml_serializer, toml_data)
  match deserialized_toml {
    Ok(config) => {
      assert_eq(TelemetryConfig::get_config_id(config), "config-1")
      assert_eq(TelemetryConfig::get_sensors(config), ["sensor-1", "sensor-2"])
    }
    Error(_) => assert_true(false)
  }
}

// Test 6: Serialization Performance and Optimization
test "serialization performance and optimization" {
  // Test serialization performance
  let performance_tester = SerializationPerformanceTester::new()
  
  // Create large test dataset
  let large_dataset = []
  for i in 0..=10000 {
    large_dataset.push(TelemetryData::new(
      "sensor-" + i.to_string(),
      1234567890 + i,
      [("temperature", 25.0 + i.to_float()), ("humidity", 60.0 + i.to_float())]
    ))
  }
  
  // Test JSON serialization performance
  let json_performance = performance_tester.test_serialization(
    JsonSerializer::new(),
    large_dataset,
    "json"
  )
  
  assert_true(SerializationPerformance::get_serialization_time(json_performance) > 0)
  assert_true(SerializationPerformance::get_deserialization_time(json_performance) > 0)
  assert_true(SerializationPerformance::get_serialized_size(json_performance) > 0)
  
  // Test binary serialization performance
  let binary_performance = performance_tester.test_serialization(
    BinarySerializer::new(),
    large_dataset,
    "binary"
  )
  
  assert_true(SerializationPerformance::get_serialization_time(binary_performance) > 0)
  assert_true(SerializationPerformance::get_deserialization_time(binary_performance) > 0)
  
  // Binary should be smaller than JSON
  assert_true(
    SerializationPerformance::get_serialized_size(binary_performance) < 
    SerializationPerformance::get_serialized_size(json_performance)
  )
  
  // Test protobuf serialization performance
  let protobuf_performance = performance_tester.test_serialization(
    ProtobufSerializer::new(),
    large_dataset,
    "protobuf"
  )
  
  assert_true(SerializationPerformance::get_serialization_time(protobuf_performance) > 0)
  assert_true(SerializationPerformance::get_deserialization_time(protobuf_performance) > 0)
  
  // Test serialization optimization
  let optimizer = SerializationOptimizer::new()
  
  // Test field elimination optimization
  let optimized_serializer = SerializationOptimizer::eliminate_fields(
    optimizer,
    JsonSerializer::new(),
    ["timestamp"] // Remove timestamp field to optimize
  )
  
  let optimized_data = SerializationOptimizer::serialize(optimized_serializer, large_dataset[0])
  assert_false(optimized_data.contains("timestamp")) // Timestamp should be removed
  
  // Test compression optimization
  let compressed_serializer = SerializationOptimizer::add_compression(
    optimizer,
    BinarySerializer::new(),
    "gzip"
  )
  
  let compressed_data = SerializationOptimizer::serialize(compressed_serializer, large_dataset[0])
  let uncompressed_data = BinarySerializer::serialize(BinarySerializer::new(), large_dataset[0])
  
  assert_true(compressed_data.length() < uncompressed_data.length()) // Should be smaller
  
  // Test caching optimization
  let cache_optimizer = SerializationCacheOptimizer::new()
  
  let cached_serializer = SerializationCacheOptimizer::add_caching(
    cache_optimizer,
    JsonSerializer::new(),
    100 // Cache size of 100
  )
  
  // First serialization should cache the result
  let first_result = SerializationCacheOptimizer::serialize(cached_serializer, large_dataset[0])
  let second_result = SerializationCacheOptimizer::serialize(cached_serializer, large_dataset[0])
  
  assert_eq(first_result, second_result) // Results should be identical
  assert_true(SerializationCacheOptimizer::get_cache_hit_rate(cache_optimizer) > 0.0)
}

// Test 7: Serialization Security
test "serialization security" {
  // Test secure JSON serializer
  let secure_json_serializer = SecureJsonSerializer::new()
  
  // Test input sanitization
  let malicious_input = "{\"name\":\"<script>alert('xss')</script>\"}"
  let sanitized_result = SecureJsonSerializer::sanitize(secure_json_serializer, malicious_input)
  assert_false(sanitized_result.contains("<script>")) // Script tag should be removed
  
  // Test depth limit protection
  let deeply_nested_object = create_deeply_nested_object(100) // Create object with 100 levels of nesting
  let secure_result = SecureJsonSerializer::serialize(secure_json_serializer, deeply_nested_object)
  
  match secure_result {
    Ok(_) => assert_true(false) // Should fail due to depth limit
    Error(e) => assert_true(e.contains("Depth limit exceeded"))
  }
  
  // Test size limit protection
  let large_object = create_large_object(1000000) // Create object with 1MB of data
  let size_limited_result = SecureJsonSerializer::serialize(secure_json_serializer, large_object)
  
  match size_limited_result {
    Ok(_) => assert_true(false) // Should fail due to size limit
    Error(e) => assert_true(e.contains("Size limit exceeded"))
  }
  
  // Test secure binary serializer
  let secure_binary_serializer = SecureBinarySerializer::new()
  
  // Test encrypted serialization
  let encryption_key = EncryptionKey::generate()
  let encrypted_serializer = SecureBinarySerializer::with_encryption(
    secure_binary_serializer,
    encryption_key,
    "AES-256"
  )
  
  let sensitive_data = SensitiveData::new("user-1", "secret-password")
  let encrypted_data = SecureBinarySerializer::serialize(encrypted_serializer, sensitive_data)
  
  // Encrypted data should not contain the original password
  let encrypted_string = String::from_bytes(encrypted_data)
  assert_false(encrypted_string.contains("secret-password"))
  
  // Test decryption
  let decrypted_data = SecureBinarySerializer::deserialize(encrypted_serializer, encrypted_data)
  match decrypted_data {
    Ok(data) => {
      assert_eq(SensitiveData::get_user_id(data), "user-1")
      assert_eq(SensitiveData::get_password(data), "secret-password")
    }
    Error(_) => assert_true(false)
  }
  
  // Test signature verification
  let signed_serializer = SecureBinarySerializer::with_signature(
    secure_binary_serializer,
    encryption_key,
    "RSA-2048"
  )
  
  let signed_data = SecureBinarySerializer::serialize(signed_serializer, sensitive_data)
  
  // Tamper with the data
  let tampered_data = signed_data.slice(0, signed_data.length() - 1) + [0] // Change last byte
  
  let verification_result = SecureBinarySerializer::verify_signature(signed_serializer, tampered_data)
  assert_false(verification_result) // Should fail verification
}

// Test 8: Serialization Versioning and Migration
test "serialization versioning and migration" {
  // Test versioned serializer
  let versioned_serializer = VersionedSerializer::new()
  
  // Test v1 serialization
  let telemetry_v1 = TelemetryV1::new("sensor-1", 1234567890, 25.5)
  let v1_data = VersionedSerializer::serialize(versioned_serializer, telemetry_v1, "1.0")
  
  assert_true(v1_data.contains("\"version\":\"1.0\""))
  assert_true(v1_data.contains("\"sensorId\":\"sensor-1\""))
  assert_true(v1_data.contains("\"temperature\":25.5"))
  
  // Test v2 serialization (with additional field)
  let telemetry_v2 = TelemetryV2::new("sensor-1", 1234567890, 25.5, "location-1")
  let v2_data = VersionedSerializer::serialize(versioned_serializer, telemetry_v2, "2.0")
  
  assert_true(v2_data.contains("\"version\":\"2.0\""))
  assert_true(v2_data.contains("\"sensorId\":\"sensor-1\""))
  assert_true(v2_data.contains("\"temperature\":25.5"))
  assert_true(v2_data.contains("\"location\":\"location-1\""))
  
  // Test backward compatibility (v2 data to v1 object)
  let backward_compatible = VersionedSerializer::deserialize[TelemetryV1](versioned_serializer, v2_data)
  match backward_compatible {
    Ok(v1_obj) => {
      assert_eq(TelemetryV1::get_sensor_id(v1_obj), "sensor-1")
      assert_eq(TelemetryV1::get_temperature(v1_obj), 25.5)
      // Location field should be ignored
    }
    Error(_) => assert_true(false)
  }
  
  // Test forward compatibility (v1 data to v2 object)
  let forward_compatible = VersionedSerializer::deserialize[TelemetryV2](versioned_serializer, v1_data)
  match forward_compatible {
    Ok(v2_obj) => {
      assert_eq(TelemetryV2::get_sensor_id(v2_obj), "sensor-1")
      assert_eq(TelemetryV2::get_temperature(v2_obj), 25.5)
      assert_eq(TelemetryV2::get_location(v2_obj), "") // Location should be default value
    }
    Error(_) => assert_true(false)
  }
  
  // Test migration
  let migration_manager = MigrationManager::new()
  
  // Add migration from v1 to v2
  MigrationManager::add_migration(migration_manager, "1.0", "2.0", fn(v1_data) {
    TelemetryV2::new(
      TelemetryV1::get_sensor_id(v1_data),
      TelemetryV1::get_timestamp(v1_data),
      TelemetryV1::get_temperature(v1_data),
      "default-location" // Set default location
    )
  })
  
  // Test automatic migration
  let migrated_data = MigrationManager::migrate(migration_manager, v1_data, "2.0")
  match migrated_data {
    Ok(v2_obj) => {
      assert_eq(TelemetryV2::get_sensor_id(v2_obj), "sensor-1")
      assert_eq(TelemetryV2::get_location(v2_obj), "default-location")
    }
    Error(_) => assert_true(false)
  }
  
  // Test batch migration
  let v1_data_batch = [v1_data, v1_data, v1_data]
  let migrated_batch = MigrationManager::migrate_batch(migration_manager, v1_data_batch, "2.0")
  
  match migrated_batch {
    Ok(batch) => assert_eq(batch.length(), 3)
    Error(_) => assert_true(false)
  }
}

// Test 9: Serialization Error Handling and Recovery
test "serialization error handling and recovery" {
  // Test resilient serializer
  let resilient_serializer = ResilientSerializer::new()
  
  // Test handling of corrupted data
  let corrupted_json = "{\"sensorId\":\"sensor-1\",\"temperature\":invalid}"
  let recovery_result = ResilientSerializer::deserialize_with_recovery[TelemetryData](
    resilient_serializer,
    corrupted_json,
    fn(error) {
      // Try to fix the data by replacing invalid value with default
      let fixed_json = corrupted_json.replace("invalid", "0.0")
      JsonSerializer::deserialize[TelemetryData](JsonSerializer::new(), fixed_json)
    }
  )
  
  match recovery_result {
    Ok(data) => {
      assert_eq(TelemetryData::get_sensor_id(data), "sensor-1")
      assert_eq(TelemetryData::get_temperature(data), 0.0) // Should be default value
    }
    Error(_) => assert_true(false)
  }
  
  // Test partial data recovery
  let partial_json = "{\"sensorId\":\"sensor-1\"}" // Missing temperature field
  let partial_recovery = ResilientSerializer::deserialize_with_recovery[TelemetryData](
    resilient_serializer,
    partial_json,
    fn(error) {
      // Add missing field with default value
      let fixed_json = partial_json.replace("}", ",\"temperature\":0.0}")
      JsonSerializer::deserialize[TelemetryData](JsonSerializer::new(), fixed_json)
    }
  )
  
  match partial_recovery {
    Ok(data) => {
      assert_eq(TelemetryData::get_sensor_id(data), "sensor-1")
      assert_eq(TelemetryData::get_temperature(data), 0.0) // Should be default value
    }
    Error(_) => assert_true(false)
  }
  
  // Test serialization error logging
  let error_logger = SerializationErrorLogger::new()
  
  let error_result = JsonSerializer::deserialize[TelemetryData](JsonSerializer::new(), "invalid json")
  match error_result {
    Ok(_) => assert_true(false)
    Error(e) => {
      SerializationErrorLogger::log_error(error_logger, e, "invalid json")
      
      let error_logs = SerializationErrorLogger::get_logs(error_logger)
      assert_eq(error_logs.length(), 1)
      assert_true(error_logs[0].contains("Invalid JSON"))
    }
  }
  
  // Test serialization retry mechanism
  let retry_serializer = RetrySerializer::new(
    JsonSerializer::new(),
    3, // Max retries
    100 // Delay between retries in ms
  )
  
  let mut attempt_count = 0
  let retry_result = RetrySerializer::deserialize_with_retry[TelemetryData](
    retry_serializer,
    corrupted_json,
    fn() {
      attempt_count = attempt_count + 1
      if attempt_count < 3 {
        // Fail first two attempts
        Error("Still corrupted")
      } else {
        // Succeed on third attempt
        JsonSerializer::deserialize[TelemetryData](JsonSerializer::new(), "{\"sensorId\":\"sensor-1\",\"temperature\":25.5}")
      }
    }
  )
  
  match retry_result {
    Ok(data) => {
      assert_eq(TelemetryData::get_sensor_id(data), "sensor-1")
      assert_eq(attempt_count, 3) // Should have attempted 3 times
    }
    Error(_) => assert_true(false)
  }
}

// Test 10: Serialization Testing and Validation
test "serialization testing and validation" {
  // Test serialization validator
  let serialization_validator = SerializationValidator::new()
  
  // Test round-trip validation
  let original_data = TelemetryData::new(
    "sensor-1",
    1234567890,
    [("temperature", 25.5), ("humidity", 60.0), ("pressure", 1013.25)]
  )
  
  let json_serializer = JsonSerializer::new()
  let binary_serializer = BinarySerializer::new()
  
  // Test JSON round-trip
  let json_roundtrip_result = SerializationValidator::validate_roundtrip(
    serialization_validator,
    original_data,
    json_serializer
  )
  
  match json_roundtrip_result {
    Ok(data) => {
      assert_eq(TelemetryData::get_sensor_id(data), "sensor-1")
      assert_eq(TelemetryData::get_timestamp(data), 1234567890)
      assert_eq(TelemetryData::get_metrics(data).get("temperature"), Some(25.5))
    }
    Error(_) => assert_true(false)
  }
  
  // Test binary round-trip
  let binary_roundtrip_result = SerializationValidator::validate_roundtrip(
    serialization_validator,
    original_data,
    binary_serializer
  )
  
  match binary_roundtrip_result {
    Ok(data) => {
      assert_eq(TelemetryData::get_sensor_id(data), "sensor-1")
      assert_eq(TelemetryData::get_timestamp(data), 1234567890)
      assert_eq(TelemetryData::get_metrics(data).get("temperature"), Some(25.5))
    }
    Error(_) => assert_true(false)
  }
  
  // Test serialization compatibility
  let compatibility_tester = SerializationCompatibilityTester::new()
  
  let v1_serializer = JsonSerializer::new()
  let v2_serializer = JsonSerializer::new()
  
  // Test forward compatibility (v1 serializer, v2 deserializer)
  let forward_compatibility = SerializationCompatibilityTester::test_forward_compatibility(
    compatibility_tester,
    v1_serializer,
    v2_serializer,
    original_data
  )
  
  assert_true(forward_compatibility.is_compatible)
  
  // Test backward compatibility (v2 serializer, v1 deserializer)
  let backward_compatibility = SerializationCompatibilityTester::test_backward_compatibility(
    compatibility_tester,
    v2_serializer,
    v1_serializer,
    original_data
  )
  
  assert_true(backward_compatibility.is_compatible)
  
  // Test serialization fuzzing
  let fuzz_tester = SerializationFuzzTester::new()
  
  let fuzz_result = SerializationFuzzTester::fuzz_deserialize(
    fuzz_tester,
    json_serializer,
    1000 // 1000 iterations
  )
  
  assert_true(SerializationFuzzResult::get_total_tests(fuzz_result) == 1000)
  assert_true(SerializationFuzzResult::get_passed_tests(fuzz_result) > 900) // At least 90% should pass
  assert_true(SerializationFuzzResult::get_crashes(fuzz_result) == 0) // No crashes should occur
  
  // Test serialization benchmarking
  let benchmark_runner = SerializationBenchmarkRunner::new()
  
  let benchmark_result = SerializationBenchmarkRunner::run_benchmark(
    benchmark_runner,
    [json_serializer, binary_serializer],
    original_data,
    100 // 100 iterations
  )
  
  assert_eq(SerializationBenchmarkResult::get_serializer_count(benchmark_result), 2)
  assert_true(SerializationBenchmarkResult::get_average_serialization_time(benchmark_result, "json") > 0)
  assert_true(SerializationBenchmarkResult::get_average_serialization_time(benchmark_result, "binary") > 0)
  
  // Test serialization report generation
  let report_generator = SerializationReportGenerator::new()
  
  let report = SerializationReportGenerator::generate_report(
    report_generator,
    [json_roundtrip_result, binary_roundtrip_result],
    [forward_compatibility, backward_compatibility],
    [fuzz_result],
    [benchmark_result]
  )
  
  assert_true(report.contains("Serialization Test Report"))
  assert_true(report.contains("Round-trip Validation"))
  assert_true(report.contains("Compatibility Testing"))
  assert_true(report.contains("Fuzz Testing"))
  assert_true(report.contains("Performance Benchmarking"))
}

// Helper functions for creating test data
fn create_deeply_nested_object(depth : Int) -> TestObject {
  if depth <= 0 {
    TestObject::new("leaf", 0, true)
  } else {
    let nested = create_deeply_nested_object(depth - 1)
    TestObject::new("node-" + depth.to_string(), depth, true)
  }
}

fn create_large_object(size : Int) -> TestObject {
  let large_string = "x" * size
  TestObject::new("large", size, true)
}