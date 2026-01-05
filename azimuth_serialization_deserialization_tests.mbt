// Azimuth Serialization Deserialization Tests
// This file contains test cases for serialization and deserialization functionality

// Test 1: Basic Serialization and Deserialization
test "basic serialization and deserialization" {
  let serializer = JsonSerializer::new()
  
  // Test primitive types
  let int_value = 42
  let serialized_int = serializer.serialize(int_value)
  let deserialized_int = serializer.deserialize_int(serialized_int)
  assert_eq(deserialized_int, int_value)
  
  let float_value = 3.14159
  let serialized_float = serializer.serialize(float_value)
  let deserialized_float = serializer.deserialize_float(serialized_float)
  assert_eq(deserialized_float, float_value)
  
  let string_value = "test_string"
  let serialized_string = serializer.serialize(string_value)
  let deserialized_string = serializer.deserialize_string(serialized_string)
  assert_eq(deserialized_string, string_value)
  
  let bool_value = true
  let serialized_bool = serializer.serialize(bool_value)
  let deserialized_bool = serializer.deserialize_bool(serialized_bool)
  assert_eq(deserialized_bool, bool_value)
  
  // Test array serialization
  let array_value = [1, 2, 3, 4, 5]
  let serialized_array = serializer.serialize(array_value)
  let deserialized_array = serializer.deserialize_int_array(serialized_array)
  assert_eq(deserialized_array, array_value)
  
  // Test object serialization
  let test_object = {
    "name": "test_object",
    "value": 123,
    "active": true,
    "tags": ["tag1", "tag2", "tag3"]
  }
  
  let serialized_object = serializer.serialize_object(test_object)
  let deserialized_object = serializer.deserialize_object(serialized_object)
  
  assert_eq(deserialized_object.get("name"), Some("test_object"))
  assert_eq(deserialized_object.get("value"), Some(123))
  assert_eq(deserialized_object.get("active"), Some(true))
  assert_eq(deserialized_object.get("tags"), Some(["tag1", "tag2", "tag3"]))
}

// Test 2: Binary Serialization Formats
test "binary serialization formats" {
  // Test MessagePack serialization
  let msgpack_serializer = MessagePackSerializer::new()
  
  let complex_data = {
    "user_id": 12345,
    "username": "test_user",
    "preferences": {
      "theme": "dark",
      "notifications": true
    },
    "scores": [95, 87, 92, 88],
    "metadata": null
  }
  
  let msgpack_data = msgpack_serializer.serialize(complex_data)
  
  // Verify it's binary data
  assert_eq(msgpack_data.length() > 0, true)
  
  // Deserialize and verify
  let deserialized_data = msgpack_serializer.deserialize(msgpack_data)
  assert_eq(deserialized_data.get("user_id"), Some(12345))
  assert_eq(deserialized_data.get("username"), Some("test_user"))
  
  let preferences = deserialized_data.get("preferences")
  match preferences {
    Some(pref_obj) => {
      assert_eq(pref_obj.get("theme"), Some("dark"))
      assert_eq(pref_obj.get("notifications"), Some(true))
    }
    _ => assert_true(false)
  }
  
  // Test Protocol Buffers serialization
  let proto_serializer = ProtobufSerializer::new()
  
  // Define a test message
  let test_message = {
    "id": 1001,
    "name": "test_message",
    "timestamp": 1640995200000L,
    "data": "binary_data_here"
  }
  
  let proto_data = proto_serializer.serialize(test_message)
  
  // Protocol Buffers should produce compact binary data
  assert_eq(proto_data.length() < msgpack_data.length(), true)
  
  let deserialized_proto = proto_serializer.deserialize(proto_data)
  assert_eq(deserialized_proto.get("id"), Some(1001))
  assert_eq(deserialized_proto.get("name"), Some("test_message"))
  
  // Test CBOR serialization
  let cbor_serializer = CborSerializer::new()
  
  let cbor_data = cbor_serializer.serialize(complex_data)
  let deserialized_cbor = cbor_serializer.deserialize(cbor_data)
  
  assert_eq(deserialized_cbor.get("user_id"), Some(12345))
  assert_eq(deserialized_cbor.get("username"), Some("test_user"))
  
  // Compare sizes of different formats
  let json_size = JsonSerializer::new().serialize(complex_data).length()
  let msgpack_size = msgpack_data.length()
  let proto_size = proto_data.length()
  let cbor_size = cbor_data.length()
  
  // Binary formats should be smaller than JSON
  assert_eq(msgpack_size < json_size, true)
  assert_eq(proto_size < json_size, true)
  assert_eq(cbor_size < json_size, true)
}

// Test 3: Custom Serialization and Deserialization
test "custom serialization and deserialization" {
  // Define custom serializable type
  type User = {
    id: Int,
    name: String,
    email: String,
    created_at: Int64
  }
  
  let user = User {
    id: 123,
    name: "John Doe",
    email: "john@example.com",
    created_at: 1640995200000L
  }
  
  // Implement custom serializer
  let user_serializer = CustomSerializer::new()
  
  user_serializer.add_serializer("User", fn(user) {
    return {
      "id": user.id,
      "name": user.name,
      "email": user.email,
      "created_at": user.created_at
    }
  })
  
  user_serializer.add_deserializer("User", fn(data) {
    match (data.get("id"), data.get("name"), data.get("email"), data.get("created_at")) {
      (Some(id), Some(name), Some(email), Some(created_at)) => {
        return User {
          id: id,
          name: name,
          email: email,
          created_at: created_at
        }
      }
      _ => raise SerializationError::new("Invalid User data")
    }
  })
  
  // Serialize and deserialize
  let serialized_user = user_serializer.serialize_custom("User", user)
  let deserialized_user = user_serializer.deserialize_custom("User", serialized_user)
  
  assert_eq(deserialized_user.id, user.id)
  assert_eq(deserialized_user.name, user.name)
  assert_eq(deserialized_user.email, user.email)
  assert_eq(deserialized_user.created_at, user.created_at)
  
  // Test versioning support
  let versioned_serializer = VersionedSerializer::new()
  
  versioned_serializer.add_version("User", 1, fn(user) {
    return {
      "id": user.id,
      "name": user.name,
      "email": user.email,
      "created_at": user.created_at,
      "version": 1
    }
  })
  
  versioned_serializer.add_version("User", 2, fn(user) {
    return {
      "id": user.id,
      "name": user.name,
      "email": user.email,
      "created_at": user.created_at,
      "updated_at": Clock::now(),
      "version": 2
    }
  })
  
  // Serialize with version 2
  let v2_data = versioned_serializer.serialize_versioned("User", 2, user)
  
  // Deserialize with version compatibility
  let deserialized_v2 = versioned_serializer.deserialize_compatible("User", v2_data)
  assert_eq(deserialized_v2.get("version"), Some(2))
  
  // Test field transformation during serialization
  let transforming_serializer = TransformingSerializer::new()
  
  transforming_serializer.add_transform("User", "email", fn(email) {
    // Encrypt email during serialization
    return encrypt_string(email)
  })
  
  transforming_serializer.add_reverse_transform("User", "email", fn(encrypted_email) {
    // Decrypt email during deserialization
    return decrypt_string(encrypted_email)
  })
  
  let transformed_data = transforming_serializer.serialize_with_transforms("User", user)
  let restored_user = transforming_serializer.deserialize_with_transforms("User", transformed_data)
  
  assert_eq(restored_user.email, user.email)  // Should be the same after decryption
}

// Test 4: Serialization Performance and Optimization
test "serialization performance and optimization" {
  let performance_tester = SerializationPerformanceTester::new()
  
  // Create large test data
  let large_data = []
  for i = 0; i < 10000; i = i + 1 {
    large_data.push({
      "id": i,
      "name": "item_" + i.to_string(),
      "value": i * 2.5,
      "active": i % 2 == 0,
      "tags": ["tag" + (i % 10).to_string(), "category" + (i % 5).to_string()]
    })
  }
  
  // Test JSON serialization performance
  let json_serializer = JsonSerializer::new()
  let json_time = performance_tester.measure_serialization(json_serializer, large_data)
  
  // Test MessagePack serialization performance
  let msgpack_serializer = MessagePackSerializer::new()
  let msgpack_time = performance_tester.measure_serialization(msgpack_serializer, large_data)
  
  // Test Protocol Buffers serialization performance
  let proto_serializer = ProtobufSerializer::new()
  let proto_time = performance_tester.measure_serialization(proto_serializer, large_data)
  
  // Binary formats should be faster than JSON
  assert_eq(msgpack_time < json_time, true)
  assert_eq(proto_time < json_time, true)
  
  // Test streaming serialization for large data
  let stream_serializer = StreamSerializer::new()
  
  let stream_start_time = Clock::now()
  let stream = stream_serializer.start_stream()
  
  // Process data in chunks
  for i = 0; i < large_data.length(); i = i + 100 {
    let chunk = large_data.slice(i, i + 100)
    stream_serializer.write_chunk(stream, chunk)
  }
  
  let stream_data = stream_serializer.finish_stream(stream)
  let stream_end_time = Clock::now()
  let stream_time = stream_end_time - stream_start_time
  
  // Streaming should be memory efficient for large data
  assert_eq(stream_time < json_time * 2, true)  // Should be competitive
  
  // Test serialization with compression
  let compressed_serializer = CompressedSerializer::new(json_serializer, CompressionAlgorithm::Gzip)
  
  let compressed_time = performance_tester.measure_serialization(compressed_serializer, large_data)
  let compressed_data = compressed_serializer.serialize(large_data)
  let uncompressed_data = json_serializer.serialize(large_data)
  
  // Compressed data should be smaller
  assert_eq(compressed_data.length() < uncompressed_data.length(), true)
  
  // Test deserialization performance
  let json_deserialize_time = performance_tester.measure_deserialization(json_serializer, uncompressed_data)
  let msgpack_deserialize_time = performance_tester.measure_deserialization(msgpack_serializer, msgpack_serializer.serialize(large_data))
  
  // Binary formats should deserialize faster
  assert_eq(msgpack_deserialize_time < json_deserialize_time, true)
}

// Test 5: Serialization Error Handling and Validation
test "serialization error handling and validation" {
  let validator = SerializationValidator::new()
  
  // Add validation rules
  validator.add_rule("user.email", ValidationRule::email())
  validator.add_rule("user.age", ValidationRule::range(0, 150))
  validator.add_rule("user.name", ValidationRule::min_length(2))
  
  let validating_serializer = ValidatingSerializer::new(JsonSerializer::new(), validator)
  
  // Test valid data
  let valid_user = {
    "email": "user@example.com",
    "age": 25,
    "name": "John Doe"
  }
  
  let valid_result = validating_serializer.serialize(valid_user)
  assert_eq(valid_result.is_ok(), true)
  
  // Test invalid data
  let invalid_user = {
    "email": "invalid-email",
    "age": 200,
    "name": "A"
  }
  
  let invalid_result = validating_serializer.serialize(invalid_user)
  assert_eq(invalid_result.is_err(), true)
  
  match invalid_result {
    Err(error) => {
      assert_eq(error.errors.length(), 3)  // All three fields should have errors
    }
    _ => assert_true(false)
  }
  
  // Test partial serialization with error collection
  let partial_serializer = PartialSerializer::new()
  
  let partial_result = partial_serializer.serialize_with_errors(invalid_user)
  assert_eq(partial_result.successful_fields.length(), 0)
  assert_eq(partial_result.errors.length(), 3)
  
  // Test circular reference detection
  let circular_detector = CircularReferenceDetector::new()
  
  let obj1 = {}
  let obj2 = {}
  obj1["reference"] = obj2
  obj2["reference"] = obj1  // Circular reference
  
  let circular_serializer = CircularSafeSerializer::new(JsonSerializer::new(), circular_detector)
  
  let circular_result = circular_serializer.serialize(obj1)
  assert_eq(circular_result.is_err(), true)
  
  match circular_result {
    Err(error) => {
      assert_eq(error.type, SerializationError::CircularReference)
    }
    _ => assert_true(false)
  }
  
  // Test schema validation
  let schema = {
    "type": "object",
    "properties": {
      "name": {"type": "string"},
      "age": {"type": "number", "minimum": 0},
      "email": {"type": "string", "format": "email"}
    },
    "required": ["name", "age"]
  }
  
  let schema_validator = SchemaValidator::new(schema)
  let schema_serializer = SchemaValidatingSerializer::new(JsonSerializer::new(), schema_validator)
  
  let valid_schema_data = {
    "name": "Jane Doe",
    "age": 30,
    "email": "jane@example.com"
  }
  
  let schema_valid_result = schema_serializer.serialize(valid_schema_data)
  assert_eq(schema_valid_result.is_ok(), true)
  
  let invalid_schema_data = {
    "name": "Jane Doe",
    "age": -5,  // Invalid age
    "email": "not-an-email"
  }
  
  let schema_invalid_result = schema_serializer.serialize(invalid_schema_data)
  assert_eq(schema_invalid_result.is_err(), true)
}

// Test 6: Cross-Platform Serialization
test "cross platform serialization" {
  // Test endianness handling
  let endianness_converter = EndiannessConverter::new()
  
  let test_number = 0x12345678
  let little_endian_bytes = endianness_converter.to_little_endian(test_number)
  let big_endian_bytes = endianness_converter.to_big_endian(test_number)
  
  // Bytes should be different
  assert_eq(little_endian_bytes[0] != big_endian_bytes[0], true)
  
  // Convert back should get original
  let from_little = endianness_converter.from_little_endian(little_endian_bytes)
  let from_big = endianness_converter.from_big_endian(big_endian_bytes)
  
  assert_eq(from_little, test_number)
  assert_eq(from_big, test_number)
  
  // Test platform-independent serialization
  let portable_serializer = PortableSerializer::new()
  
  let portable_data = {
    "timestamp": 1640995200000L,
    "value": 3.14159,
    "flag": true,
    "data": [1, 2, 3, 4, 5]
  }
  
  let portable_bytes = portable_serializer.serialize(portable_data)
  
  // Simulate different platform deserialization
  let restored_data = portable_serializer.deserialize(portable_bytes)
  
  assert_eq(restored_data.get("timestamp"), Some(1640995200000L))
  assert_eq(restored_data.get("value"), Some(3.14159))
  assert_eq(restored_data.get("flag"), Some(true))
  assert_eq(restored_data.get("data"), Some([1, 2, 3, 4, 5]))
  
  // Test language-agnostic serialization format
  let language_agnostic = LanguageAgnosticSerializer::new()
  
  let complex_object = {
    "nested": {
      "array": [1.1, 2.2, 3.3],
      "boolean": false
    },
    "string": "unicode_test_中文字符",
    "null_value": null,
    "number": 42
  }
  
  let agnostic_data = language_agnostic.serialize(complex_object)
  
  // Should be deserializable in any language with the same spec
  let agnostic_restored = language_agnostic.deserialize(agnostic_data)
  
  let nested = agnostic_restored.get("nested")
  match nested {
    Some(nested_obj) => {
      assert_eq(nested_obj.get("array"), Some([1.1, 2.2, 3.3]))
      assert_eq(nested_obj.get("boolean"), Some(false))
    }
    _ => assert_true(false)
  }
  
  assert_eq(agnostic_restored.get("string"), Some("unicode_test_中文字符"))
  assert_eq(agnostic_restored.get("null_value"), None)
  assert_eq(agnostic_restored.get("number"), Some(42))
}

// Test 7: Serialization Security and Encryption
test "serialization security and encryption" {
  // Test encrypted serialization
  let encryption_key = EncryptionKey::generate()
  let encrypted_serializer = EncryptedSerializer::new(JsonSerializer::new(), encryption_key)
  
  let sensitive_data = {
    "username": "admin",
    "password": "secret_password",
    "api_key": "sk-1234567890abcdef",
    "credit_card": "4111-1111-1111-1111"
  }
  
  let encrypted_data = encrypted_serializer.serialize(sensitive_data)
  
  // Encrypted data should not be readable as plain text
  assert_eq(encrypted_data.contains("admin"), false)
  assert_eq(encrypted_data.contains("secret_password"), false)
  
  // Decrypt and verify
  let decrypted_data = encrypted_serializer.deserialize(encrypted_data)
  
  assert_eq(decrypted_data.get("username"), Some("admin"))
  assert_eq(decrypted_data.get("password"), Some("secret_password"))
  
  // Test with wrong key (should fail)
  let wrong_key = EncryptionKey::generate()
  let wrong_serializer = EncryptedSerializer::new(JsonSerializer::new(), wrong_key)
  
  let decrypt_result = wrong_serializer.deserialize(encrypted_data)
  assert_eq(decrypt_result.is_err(), true)
  
  // Test data masking during serialization
  let masking_rules = [
    ("password", MaskingRule::replace_with("*****")),
    ("credit_card", MaskingRule::keep_last(4)),
    ("api_key", MaskingRule::pattern("sk-****"))
  ]
  
  let masking_serializer = MaskingSerializer::new(JsonSerializer::new(), masking_rules)
  
  let masked_data = masking_serializer.serialize(sensitive_data)
  let masked_deserialized = JsonSerializer::new().deserialize(masked_data)
  
  assert_eq(masked_deserialized.get("password"), Some("*****"))
  assert_eq(masked_deserialized.get("credit_card"), Some("****-1111"))
  assert_eq(masked_deserialized.get("api_key"), Some("sk-****"))
  assert_eq(masked_deserialized.get("username"), Some("admin"))  // Not masked
  
  // Test serialization with digital signature
  let signing_key = SigningKey::generate()
  let signed_serializer = SignedSerializer::new(JsonSerializer::new(), signing_key)
  
  let signed_data = signed_serializer.serialize(sensitive_data)
  
  // Verify signature
  let verification_result = signed_serializer.verify(signed_data)
  assert_eq(verification_result, true)
  
  // Tamper with data and verify signature fails
  let tampered_data = signed_data.replace("admin", "hacker")
  let tampered_result = signed_serializer.verify(tampered_data)
  assert_eq(tampered_result, false)
  
  // Test secure serialization with audit log
  let audit_logger = SerializationAuditLogger::new()
  let secure_serializer = SecureSerializer::new(encrypted_serializer, audit_logger)
  
  let audit_data = {"operation": "transfer", "amount": 10000}
  let audit_result = secure_serializer.serialize(audit_data)
  
  // Check audit log
  let audit_entries = audit_logger.get_entries()
  assert_eq(audit_entries.length(), 1)
  assert_eq(audit_entries[0].operation, "serialize")
  assert_eq(audit_entries[0].data_type, "object")
  assert_eq(audit_entries[0].timestamp > 0, true)
}

// Test 8: Serialization Caching and Memoization
test "serialization caching and memoization" {
  let cache_serializer = CachedSerializer::new(JsonSerializer::new())
  
  // Test data that will be serialized multiple times
  let repeatable_data = {
    "id": 12345,
    "name": "repeatable_object",
    "config": {
      "setting1": true,
      "setting2": false,
      "setting3": 42
    }
  }
  
  // First serialization
  let start_time1 = Clock::now()
  let result1 = cache_serializer.serialize(repeatable_data)
  let end_time1 = Clock::now()
  let time1 = end_time1 - start_time1
  
  // Second serialization (should be cached)
  let start_time2 = Clock::now()
  let result2 = cache_serializer.serialize(repeatable_data)
  let end_time2 = Clock::now()
  let time2 = end_time2 - start_time2
  
  // Results should be identical
  assert_eq(result1, result2)
  
  // Second serialization should be faster
  assert_eq(time2 < time1, true)
  
  // Check cache statistics
  let cache_stats = cache_serializer.get_cache_statistics()
  assert_eq(cache_stats.hits, 1)
  assert_eq(cache_stats.misses, 1)
  assert_eq(cache_stats.size, 1)
  
  // Test memoization for deserialization
  let memoized_deserializer = MemoizedDeserializer::new(JsonSerializer::new())
  
  // Deserialize same data multiple times
  let start_time3 = Clock::now()
  let deserialized1 = memoized_deserializer.deserialize(result1)
  let end_time3 = Clock::now()
  let time3 = end_time3 - start_time3
  
  let start_time4 = Clock::now()
  let deserialized2 = memoized_deserializer.deserialize(result1)
  let end_time4 = Clock::now()
  let time4 = end_time4 - start_time4
  
  // Second deserialization should be faster
  assert_eq(time4 < time3, true)
  
  // Results should be identical
  assert_eq(deserialized1.get("id"), deserialized2.get("id"))
  
  // Test cache eviction
  cache_serializer.set_cache_limit(1)  // Limit to 1 cached item
  
  let different_data = {"id": 67890, "name": "different_object"}
  cache_serializer.serialize(different_data)
  
  // Original item should be evicted
  let cache_stats_after = cache_serializer.get_cache_statistics()
  assert_eq(cache_stats_after.size, 1)
  assert_eq(cache_stats_after.evictions, 1)
  
  // Test intelligent caching based on object size and frequency
  let intelligent_cache = IntelligentCache::new()
  intelligent_cache.configure(
    max_size: 1024 * 1024,  // 1MB
    max_items: 100,
    min_frequency: 2  // Cache items used at least twice
  )
  
  let intelligent_serializer = IntelligentCachedSerializer::new(
    JsonSerializer::new(),
    intelligent_cache
  )
  
  // Use small object frequently
  let small_object = {"small": "data"}
  for i = 0; i < 5; i = i + 1 {
    intelligent_serializer.serialize(small_object)
  }
  
  // Use large object infrequently
  let large_object = {"large": "x" * 10000}  // 10KB
  intelligent_serializer.serialize(large_object)
  
  let intelligent_stats = intelligent_serializer.get_cache_statistics()
  
  // Small object should be cached (used frequently)
  assert_eq(intelligent_stats.has_cached(small_object), true)
  
  // Large object might not be cached (too large and only used once)
  // This depends on the intelligent cache implementation
}

// Test 9: Serialization Streaming and Batch Processing
test "serialization streaming and batch processing" {
  // Test streaming serialization for large datasets
  let stream_serializer = StreamSerializer::new()
  
  let large_dataset = []
  for i = 0; i < 100000; i = i + 1 {
    large_dataset.push({
      "id": i,
      "value": i * 1.5,
      "active": i % 2 == 0,
      "category": "cat_" + (i % 10).to_string()
    })
  }
  
  // Stream serialize in chunks
  let stream = stream_serializer.start_stream()
  
  let chunk_size = 1000
  for i = 0; i < large_dataset.length(); i = i + chunk_size {
    let end = if i + chunk_size > large_dataset.length() { large_dataset.length() } else { i + chunk_size }
    let chunk = large_dataset.slice(i, end)
    stream_serializer.write_chunk(stream, chunk)
  }
  
  let streamed_data = stream_serializer.finish_stream(stream)
  
  // Verify stream contains all data
  let stream_deserializer = StreamDeserializer::new()
  let deserialization_stream = stream_deserializer.start_stream(streamed_data)
  
  let mut total_items = 0
  while !stream_deserializer.is_end(deserialization_stream) {
    let chunk = stream_deserializer.read_chunk(deserialization_stream, 1000)
    total_items = total_items + chunk.length()
  }
  
  assert_eq(total_items, 100000)
  
  // Test batch serialization
  let batch_serializer = BatchSerializer::new(JsonSerializer::new())
  
  let batch_data = [
    {"type": "user", "id": 1, "name": "Alice"},
    {"type": "user", "id": 2, "name": "Bob"},
    {"type": "product", "id": 101, "name": "Widget"},
    {"type": "product", "id": 102, "name": "Gadget"},
    {"type": "order", "id": 1001, "user_id": 1, "product_id": 101}
  ]
  
  let batched_data = batch_serializer.serialize_batch(batch_data)
  
  // Should contain metadata about the batch
  assert_eq(batched_data.contains("batch_metadata"), true)
  assert_eq(batched_data.contains("items"), true)
  
  // Test parallel batch processing
  let parallel_processor = ParallelBatchProcessor::new(4)  // 4 worker threads
  
  let processing_function = fn(item) {
    // Simulate some processing
    let processed = item
    processed["processed_at"] = Clock::now()
    return processed
  }
  
  let processed_batch = parallel_processor.process_batch(batch_data, processing_function)
  
  assert_eq(processed_batch.length(), batch_data.length())
  
  // Verify all items were processed
  for item in processed_batch {
    assert_eq(item.contains("processed_at"), true)
  }
  
  // Test incremental serialization
  let incremental_serializer = IncrementalSerializer::new()
  
  let base_data = {"version": 1, "data": [1, 2, 3]}
  let increment1 = {"increment": [4, 5]}
  let increment2 = {"increment": [6, 7, 8]}
  
  let base_serialized = incremental_serializer.serialize_base(base_data)
  let inc1_serialized = incremental_serializer.serialize_increment(increment1)
  let inc2_serialized = incremental_serializer.serialize_increment(increment2)
  
  // Apply increments to reconstruct full data
  let reconstructed = incremental_serializer.apply_increments(
    base_serialized,
    [inc1_serialized, inc2_serialized]
  )
  
  let final_data = JsonSerializer::new().deserialize(reconstructed)
  assert_eq(final_data.get("data"), Some([1, 2, 3, 4, 5, 6, 7, 8]))
}

// Test 10: Serialization Format Conversion and Migration
test "serialization format conversion and migration" {
  // Test format conversion
  let format_converter = FormatConverter::new()
  
  let test_data = {
    "users": [
      {"id": 1, "name": "Alice", "email": "alice@example.com"},
      {"id": 2, "name": "Bob", "email": "bob@example.com"}
    ],
    "created_at": 1640995200000L,
    "active": true
  }
  
  // JSON to MessagePack
  let json_serializer = JsonSerializer::new()
  let msgpack_serializer = MessagePackSerializer::new()
  
  let json_data = json_serializer.serialize(test_data)
  let msgpack_data = format_converter.convert(json_data, "json", "msgpack")
  
  // Verify conversion worked
  let converted_back = msgpack_serializer.deserialize(msgpack_data)
  assert_eq(converted_back.get("active"), Some(true))
  
  let users = converted_back.get("users")
  match users {
    Some(user_array) => {
      assert_eq(user_array.length(), 2)
      assert_eq(user_array[0].get("name"), Some("Alice"))
    }
    _ => assert_true(false)
  }
  
  // Test schema migration
  let schema_migrator = SchemaMigrator::new()
  
  // Define version 1 schema
  let v1_schema = {
    "version": 1,
    "fields": [
      {"name": "id", "type": "int"},
      {"name": "full_name", "type": "string"},
      {"name": "age", "type": "int"}
    ]
  }
  
  // Define version 2 schema (split full_name into first_name and last_name)
  let v2_schema = {
    "version": 2,
    "fields": [
      {"name": "id", "type": "int"},
      {"name": "first_name", "type": "string"},
      {"name": "last_name", "type": "string"},
      {"name": "age", "type": "int"},
      {"name": "email", "type": "string", "default": ""}
    ]
  }
  
  schema_migrator.add_schema("user", v1_schema)
  schema_migrator.add_schema("user", v2_schema)
  
  // Add migration from v1 to v2
  schema_migrator.add_migration("user", 1, 2, fn(data) {
    let full_name = data.get("full_name")
    match full_name {
      Some(name) => {
        let parts = name.split(" ")
        data["first_name"] = parts[0]
        data["last_name"] = if parts.length() > 1 { parts[1] } else { "" }
      }
      None => {}
    }
    
    // Add default email if not present
    if !data.contains("email") {
      data["email"] = ""
    }
    
    return data
  })
  
  // Create v1 data
  let v1_data = {
    "id": 1,
    "full_name": "John Doe",
    "age": 30,
    "schema_version": 1
  }
  
  // Migrate to v2
  let v2_data = schema_migrator.migrate("user", v1_data, 2)
  
  assert_eq(v2_data.get("first_name"), Some("John"))
  assert_eq(v2_data.get("last_name"), Some("Doe"))
  assert_eq(v2_data.get("age"), Some(30))
  assert_eq(v2_data.get("email"), Some(""))
  assert_eq(v2_data.get("schema_version"), Some(2))
  
  // Test automatic migration based on version
  let auto_migrator = AutoMigrator::new(schema_migrator)
  
  let migrated_data = auto_migrator.migrate_to_latest("user", v1_data)
  assert_eq(migrated_data.get("schema_version"), Some(2))
  
  // Test backward compatibility
  let backward_compatible_serializer = BackwardCompatibleSerializer::new(
    msgpack_serializer,
    schema_migrator
  )
  
  // Serialize with latest schema
  let latest_data = {
    "id": 2,
    "first_name": "Jane",
    "last_name": "Smith",
    "age": 25,
    "email": "jane@example.com",
    "schema_version": 2
  }
  
  let serialized_latest = backward_compatible_serializer.serialize("user", latest_data)
  
  // Deserialize with backward compatibility (should work for older clients)
  let v1_compatible = backward_compatible_serializer.deserialize_compatible("user", serialized_latest, 1)
  
  // Should have v1-compatible format
  assert_eq(v1_compatible.get("full_name"), Some("Jane Smith"))
  assert_eq(v1_compatible.get("age"), Some(25))
  assert_eq(v1_compatible.contains("first_name"), false)  // v1 fields only
}