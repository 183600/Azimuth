// Azimuth Serialization Deserialization Comprehensive Tests
// This file contains comprehensive tests for serialization and deserialization operations

// Test 1: JSON Serialization and Deserialization
test "JSON serialization and deserialization" {
  // Create JSON serializer
  let json_serializer = JsonSerializer::new()
  
  // Test basic data types
  let test_data = TestData::new(
    "test_string",
    42,
    3.14159,
    true,
    [1, 2, 3, 4, 5],
    {"key1": "value1", "key2": "value2"}
  )
  
  // Serialize to JSON
  let json_string = JsonSerializer::serialize(json_serializer, test_data)
  assert_true(json_string.contains("test_string"))
  assert_true(json_string.contains("42"))
  assert_true(json_string.contains("3.14159"))
  assert_true(json_string.contains("true"))
  
  // Deserialize from JSON
  let deserialized_data = JsonSerializer::deserialize(json_serializer, json_string)
  match deserialized_data {
    Ok(data) => {
      assert_eq(data.string_field, "test_string")
      assert_eq(data.int_field, 42)
      assert_true(Math::abs(data.float_field - 3.14159) < 0.00001)
      assert_eq(data.bool_field, true)
      assert_eq(data.array_field.length(), 5)
      assert_eq(data.array_field[0], 1)
      assert_eq(data.object_field.get("key1"), Some("value1"))
      assert_eq(data.object_field.get("key2"), Some("value2"))
    }
    Error(_) => assert_true(false)
  }
  
  // Test nested objects
  let nested_data = NestedData::new(
    "outer",
    InnerData::new("inner", 100)
  )
  
  let nested_json = JsonSerializer::serialize(json_serializer, nested_data)
  let deserialized_nested = JsonSerializer::deserialize(json_serializer, nested_json)
  
  match deserialized_nested {
    Ok(data) => {
      assert_eq(data.outer_field, "outer")
      assert_eq(data.inner_field.inner_string, "inner")
      assert_eq(data.inner_field.inner_int, 100)
    }
    Error(_) => assert_true(false)
  }
  
  // Test optional fields
  let data_with_optionals = DataWithOptionals::new(
    "required_field",
    Some("optional_field"),
    None
  )
  
  let optionals_json = JsonSerializer::serialize(json_serializer, data_with_optionals)
  let deserialized_optionals = JsonSerializer::deserialize(json_serializer, optionals_json)
  
  match deserialized_optionals {
    Ok(data) => {
      assert_eq(data.required, "required_field")
      assert_eq(data.optional, Some("optional_field"))
      assert_eq(data.another_optional, None)
    }
    Error(_) => assert_true(false)
  }
  
  // Test error handling for invalid JSON
  let invalid_json = "{invalid json}"
  let error_result = JsonSerializer::deserialize(json_serializer, invalid_json)
  match error_result {
    Ok(_) => assert_true(false) // Should not succeed
    Error(error) => assert_eq(Error::type(error), "ParseError")
  }
}

// Test 2: Binary Serialization and Deserialization
test "binary serialization and deserialization" {
  // Create binary serializer
  let binary_serializer = BinarySerializer::new()
  
  // Test basic data types
  let test_data = BinaryTestData::new(
    123456789,
    987654321L,
    3.14159265359,
    "binary test string",
    [1, 2, 3, 4, 5, 6, 7, 8]
  )
  
  // Serialize to binary
  let binary_data = BinarySerializer::serialize(binary_serializer, test_data)
  assert_true(binary_data.length() > 0)
  
  // Deserialize from binary
  let deserialized_data = BinarySerializer::deserialize(binary_serializer, binary_data)
  match deserialized_data {
    Ok(data) => {
      assert_eq(data.int_field, 123456789)
      assert_eq(data.long_field, 987654321L)
      assert_true(Math::abs(data.double_field - 3.14159265359) < 0.00000000001)
      assert_eq(data.string_field, "binary test string")
      assert_eq(data.byte_array.length(), 8)
      assert_eq(data.byte_array[0], 1)
      assert_eq(data.byte_array[7], 8)
    }
    Error(_) => assert_true(false)
  }
  
  // Test version compatibility
  let versioned_serializer = BinarySerializer::with_version(2)
  let versioned_data = VersionedData::new(1, "legacy data")
  
  let versioned_binary = BinarySerializer::serialize(versioned_serializer, versioned_data)
  let deserialized_versioned = BinarySerializer::deserialize(versioned_serializer, versioned_binary)
  
  match deserialized_versioned {
    Ok(data) => {
      assert_eq(data.version, 1)
      assert_eq(data.payload, "legacy data")
    }
    Error(_) => assert_true(false)
  }
  
  // Test compression
  let large_data = LargeData::new(create_large_string(10000)) // 10KB string
  let compressed_binary = BinarySerializer::serialize_with_compression(binary_serializer, large_data)
  let uncompressed_binary = BinarySerializer::serialize(binary_serializer, large_data)
  
  // Compressed data should be smaller
  assert_true(compressed_binary.length() < uncompressed_binary.length())
  
  // Deserialize from compressed data
  let decompressed_data = BinarySerializer::deserialize_with_decompression(binary_serializer, compressed_binary)
  match decompressed_data {
    Ok(data) => assert_eq(data.large_string.length(), 10000)
    Error(_) => assert_true(false)
  }
}

// Test 3: XML Serialization and Deserialization
test "XML serialization and deserialization" {
  // Create XML serializer
  let xml_serializer = XmlSerializer::new()
  
  // Test basic object serialization
  let person = Person::new(
    "John Doe",
    30,
    "john.doe@example.com",
    ["123 Main St", "Anytown", "USA"]
  )
  
  // Serialize to XML
  let xml_string = XmlSerializer::serialize(xml_serializer, person)
  assert_true(xml_string.contains("<Person>"))
  assert_true(xml_string.contains("<name>John Doe</name>"))
  assert_true(xml_string.contains("<age>30</age>"))
  assert_true(xml_string.contains("<email>john.doe@example.com</email>"))
  assert_true(xml_string.contains("<address>"))
  
  // Deserialize from XML
  let deserialized_person = XmlSerializer::deserialize(xml_serializer, xml_string)
  match deserialized_person {
    Ok(data) => {
      assert_eq(data.name, "John Doe")
      assert_eq(data.age, 30)
      assert_eq(data.email, "john.doe@example.com")
      assert_eq(data.address.length(), 3)
      assert_eq(data.address[0], "123 Main St")
    }
    Error(_) => assert_true(false)
  }
  
  // Test attributes
  let product = Product::new(
    "Laptop",
    999.99,
    ProductCategory::Electronics,
    true
  )
  
  let product_xml = XmlSerializer::serialize_with_attributes(xml_serializer, product)
  assert_true(product_xml.contains("<Product category=\"Electronics\" available=\"true\">"))
  assert_true(product_xml.contains("<name>Laptop</name>"))
  assert_true(product_xml.contains("<price>999.99</price>"))
  
  let deserialized_product = XmlSerializer::deserialize(xml_serializer, product_xml)
  match deserialized_product {
    Ok(data) => {
      assert_eq(data.name, "Laptop")
      assert_eq(data.price, 999.99)
      assert_eq(data.category, ProductCategory::Electronics)
      assert_eq(data.available, true)
    }
    Error(_) => assert_true(false)
  }
  
  // Test namespace support
  let namespaced_data = NamespacedData::new("test value")
  let namespaced_xml = XmlSerializer::serialize_with_namespace(
    xml_serializer,
    namespaced_data,
    "http://example.com/namespace"
  )
  
  assert_true(namespaced_xml.contains("xmlns=\"http://example.com/namespace\""))
  
  let deserialized_namespaced = XmlSerializer::deserialize(xml_serializer, namespaced_xml)
  match deserialized_namespaced {
    Ok(data) => assert_eq(data.value, "test value")
    Error(_) => assert_true(false)
  }
  
  // Test CDATA sections
  let cdata_data = CDataData::new("<script>alert('test');</script>")
  let cdata_xml = XmlSerializer::serialize(xml_serializer, cdata_data)
  assert_true(cdata_xml.contains("<![CDATA[<script>alert('test');</script>]]>"))
  
  let deserialized_cdata = XmlSerializer::deserialize(xml_serializer, cdata_xml)
  match deserialized_cdata {
    Ok(data) => assert_eq(data.script_content, "<script>alert('test');</script>")
    Error(_) => assert_true(false)
  }
}

// Test 4: Protocol Buffers Serialization and Deserialization
test "protocol buffers serialization and deserialization" {
  // Create protobuf serializer
  let protobuf_serializer = ProtobufSerializer::new()
  
  // Test message serialization
  let user_message = UserMessage::new(
    123,
    "testuser",
    "test@example.com",
    UserStatus::Active
  )
  
  // Serialize to protobuf
  let protobuf_data = ProtobufSerializer::serialize(protobuf_serializer, user_message)
  assert_true(protobuf_data.length() > 0)
  
  // Deserialize from protobuf
  let deserialized_user = ProtobufSerializer::deserialize(protobuf_serializer, protobuf_data)
  match deserialized_user {
    Ok(data) => {
      assert_eq(data.id, 123)
      assert_eq(data.username, "testuser")
      assert_eq(data.email, "test@example.com")
      assert_eq(data.status, UserStatus::Active)
    }
    Error(_) => assert_true(false)
  }
  
  // Test repeated fields
  let list_message = ListMessage::new([
    ItemMessage::new("item1", 10.0),
    ItemMessage::new("item2", 20.0),
    ItemMessage::new("item3", 30.0)
  ])
  
  let list_protobuf = ProtobufSerializer::serialize(protobuf_serializer, list_message)
  let deserialized_list = ProtobufSerializer::deserialize(protobuf_serializer, list_protobuf)
  
  match deserialized_list {
    Ok(data) => {
      assert_eq(data.items.length(), 3)
      assert_eq(data.items[0].name, "item1")
      assert_eq(data.items[0].value, 10.0)
      assert_eq(data.items[1].name, "item2")
      assert_eq(data.items[1].value, 20.0)
      assert_eq(data.items[2].name, "item3")
      assert_eq(data.items[2].value, 30.0)
    }
    Error(_) => assert_true(false)
  }
  
  // Test nested messages
  let nested_message = NestedMessage::new(
    "outer",
    InnerMessage::new("inner", 42)
  )
  
  let nested_protobuf = ProtobufSerializer::serialize(protobuf_serializer, nested_message)
  let deserialized_nested = ProtobufSerializer::deserialize(protobuf_serializer, nested_protobuf)
  
  match deserialized_nested {
    Ok(data) => {
      assert_eq(data.outer_field, "outer")
      assert_eq(data.inner_field.inner_string, "inner")
      assert_eq(data.inner_field.inner_int, 42)
    }
    Error(_) => assert_true(false)
  }
  
  // Test oneof fields
  let oneof_message = OneofMessage::with_string_value("string_value")
  let oneof_protobuf = ProtobufSerializer::serialize(protobuf_serializer, oneof_message)
  let deserialized_oneof = ProtobufSerializer::deserialize(protobuf_serializer, oneof_protobuf)
  
  match deserialized_oneof {
    Ok(data) => {
      match data.value {
        OneofValue::StringValue(s) => assert_eq(s, "string_value")
        _ => assert_true(false)
      }
    }
    Error(_) => assert_true(false)
  }
  
  // Test map fields
  let map_message = MapMessage::new({
    "key1": "value1",
    "key2": "value2",
    "key3": "value3"
  })
  
  let map_protobuf = ProtobufSerializer::serialize(protobuf_serializer, map_message)
  let deserialized_map = ProtobufSerializer::deserialize(protobuf_serializer, map_protobuf)
  
  match deserialized_map {
    Ok(data) => {
      assert_eq(data.key_value_map.get("key1"), Some("value1"))
      assert_eq(data.key_value_map.get("key2"), Some("value2"))
      assert_eq(data.key_value_map.get("key3"), Some("value3"))
    }
    Error(_) => assert_true(false)
  }
}

// Test 5: Custom Serialization Formats
test "custom serialization formats" {
  // Create custom serializer
  let custom_serializer = CustomSerializer::new()
  
  // Test CSV-like format
  let csv_data = CsvData::new(
    "John,Doe,30,john@example.com",
    "Jane,Smith,25,jane@example.com",
    "Bob,Johnson,40,bob@example.com"
  )
  
  let csv_string = CustomSerializer::serialize_to_csv(custom_serializer, csv_data)
  assert_true(csv_string.contains("John,Doe,30,john@example.com"))
  assert_true(csv_string.contains("Jane,Smith,25,jane@example.com"))
  assert_true(csv_string.contains("Bob,Johnson,40,bob@example.com"))
  
  let deserialized_csv = CustomSerializer::deserialize_from_csv(custom_serializer, csv_string)
  match deserialized_csv {
    Ok(data) => {
      assert_eq(data.records.length(), 3)
      assert_eq(data.records[0].first_name, "John")
      assert_eq(data.records[0].last_name, "Doe")
      assert_eq(data.records[0].age, 30)
      assert_eq(data.records[0].email, "john@example.com")
    }
    Error(_) => assert_true(false)
  }
  
  // Test key-value format
  let kv_data = KeyValueData::new({
    "name": "John Doe",
    "age": "30",
    "email": "john@example.com",
    "active": "true"
  })
  
  let kv_string = CustomSerializer::serialize_to_kv(custom_serializer, kv_data, "=")
  assert_true(kv_string.contains("name=John Doe"))
  assert_true(kv_string.contains("age=30"))
  assert_true(kv_string.contains("email=john@example.com"))
  assert_true(kv_string.contains("active=true"))
  
  let deserialized_kv = CustomSerializer::deserialize_from_kv(custom_serializer, kv_string, "=")
  match deserialized_kv {
    Ok(data) => {
      assert_eq(data.pairs.get("name"), Some("John Doe"))
      assert_eq(data.pairs.get("age"), Some("30"))
      assert_eq(data.pairs.get("email"), Some("john@example.com"))
      assert_eq(data.pairs.get("active"), Some("true"))
    }
    Error(_) => assert_true(false)
  }
  
  // Test URL-encoded format
  let url_data = UrlEncodedData::new({
    "name": "John Doe",
    "city": "New York",
    "interest": "programming",
    "interest": "music"
  })
  
  let url_string = CustomSerializer::serialize_to_url_encoded(custom_serializer, url_data)
  assert_true(url_string.contains("name=John+Doe"))
  assert_true(url_string.contains("city=New+York"))
  assert_true(url_string.contains("interest=programming"))
  assert_true(url_string.contains("interest=music"))
  
  let deserialized_url = CustomSerializer::deserialize_from_url_encoded(custom_serializer, url_string)
  match deserialized_url {
    Ok(data) => {
      assert_eq(data.params.get("name"), Some("John Doe"))
      assert_eq(data.params.get("city"), Some("New York"))
      assert_eq(data.params.get_all("interest").length(), 2)
    }
    Error(_) => assert_true(false)
  }
  
  // Test Base64 format
  let binary_data = BinaryContent::new([1, 2, 3, 4, 5, 6, 7, 8])
  let base64_string = CustomSerializer::serialize_to_base64(custom_serializer, binary_data)
  assert_true(base64_string.length() > 0)
  
  let deserialized_binary = CustomSerializer::deserialize_from_base64(custom_serializer, base64_string)
  match deserialized_binary {
    Ok(data) => {
      assert_eq(data.bytes.length(), 8)
      assert_eq(data.bytes[0], 1)
      assert_eq(data.bytes[7], 8)
    }
    Error(_) => assert_true(false)
  }
}

// Test 6: Serialization Performance and Efficiency
test "serialization performance and efficiency" {
  // Create performance test data
  let large_dataset = create_large_test_dataset(1000)
  
  // Test JSON serialization performance
  let json_serializer = JsonSerializer::new()
  let json_start_time = Time::now()
  
  let json_serialized = JsonSerializer::serialize(json_serializer, large_dataset)
  let json_serialization_time = Time::now() - json_start_time
  
  let json_deserialization_start = Time::now()
  let _ = JsonSerializer::deserialize(json_serializer, json_serialized)
  let json_deserialization_time = Time::now() - json_deserialization_start
  
  // Test binary serialization performance
  let binary_serializer = BinarySerializer::new()
  let binary_start_time = Time::now()
  
  let binary_serialized = BinarySerializer::serialize(binary_serializer, large_dataset)
  let binary_serialization_time = Time::now() - binary_start_time
  
  let binary_deserialization_start = Time::now()
  let _ = BinarySerializer::deserialize(binary_serializer, binary_serialized)
  let binary_deserialization_time = Time::now() - binary_deserialization_start
  
  // Test protobuf serialization performance
  let protobuf_serializer = ProtobufSerializer::new()
  let protobuf_start_time = Time::now()
  
  let protobuf_serialized = ProtobufSerializer::serialize(protobuf_serializer, large_dataset)
  let protobuf_serialization_time = Time::now() - protobuf_start_time
  
  let protobuf_deserialization_start = Time::now()
  let _ = ProtobufSerializer::deserialize(protobuf_serializer, protobuf_serialized)
  let protobuf_deserialization_time = Time::now() - protobuf_deserialization_start
  
  // Compare sizes
  assert_true(binary_serialized.length() < json_serialized.length())
  assert_true(protobuf_serialized.length() < json_serialized.length())
  
  // Compare performance
  assert_true(binary_serialization_time < json_serialization_time)
  assert_true(binary_deserialization_time < json_deserialization_time)
  
  // Test memory efficiency
  let json_memory_usage = MemoryProfiler::measure_usage(func() {
    JsonSerializer::serialize(json_serializer, large_dataset)
  })
  
  let binary_memory_usage = MemoryProfiler::measure_usage(func() {
    BinarySerializer::serialize(binary_serializer, large_dataset)
  })
  
  let protobuf_memory_usage = MemoryProfiler::measure_usage(func() {
    ProtobufSerializer::serialize(protobuf_serializer, large_dataset)
  })
  
  assert_true(binary_memory_usage < json_memory_usage)
  assert_true(protobuf_memory_usage < json_memory_usage)
}

// Test 7: Serialization Security and Validation
test "serialization security and validation" {
  // Create secure serializer
  let secure_serializer = SecureSerializer::new()
  
  // Test input validation
  let malicious_json = "{\"__proto__\": {\"admin\": true}}"
  let validation_result = SecureSerializer::validate_json_input(secure_serializer, malicious_json)
  match validation_result {
    Ok(_) => assert_true(false) // Should not succeed
    Error(error) => assert_eq(Error::type(error), "SecurityViolation")
  }
  
  // Test size limits
  let oversized_data = create_oversized_data(10000000) // 10MB
  let size_limit_result = SecureSerializer::serialize_with_size_limit(secure_serializer, oversized_data, 1000000) // 1MB limit
  match size_limit_result {
    Ok(_) => assert_true(false) // Should not succeed
    Error(error) => assert_eq(Error::type(error), "SizeLimitExceeded")
  }
  
  // Test depth limits
  let deeply_nested_data = create_deeply_nested_data(1000)
  let depth_limit_result = SecureSerializer::serialize_with_depth_limit(secure_serializer, deeply_nested_data, 100) // 100 levels limit
  match depth_limit_result {
    Ok(_) => assert_true(false) // Should not succeed
    Error(error) => assert_eq(Error::type(error), "DepthLimitExceeded")
  }
  
  // Test field whitelist
  let user_data = UserData::new(
    "username",
    "password123", // Sensitive field
    "user@example.com",
    "admin" // Sensitive field
  )
  
  let whitelisted_json = SecureSerializer::serialize_with_whitelist(
    secure_serializer,
    user_data,
    ["username", "email"] // Only allow these fields
  )
  
  assert_true(whitelisted_json.contains("username"))
  assert_true(whitelisted_json.contains("email"))
  assert_false(whitelisted_json.contains("password123"))
  assert_false(whitelisted_json.contains("admin"))
  
  // Test encryption
  let sensitive_data = SensitiveData::new("secret information")
  let encrypted_data = SecureSerializer::serialize_with_encryption(secure_serializer, sensitive_data, "encryption_key")
  assert_false(encrypted_data.contains("secret information")) // Should be encrypted
  
  let decrypted_data = SecureSerializer::deserialize_with_decryption(secure_serializer, encrypted_data, "encryption_key")
  match decrypted_data {
    Ok(data) => assert_eq(data.content, "secret information")
    Error(_) => assert_true(false)
  }
  
  // Test signature
  let important_data = ImportantData::new("critical information")
  let signed_data = SecureSerializer::serialize_with_signature(secure_serializer, important_data, "private_key")
  
  let verification_result = SecureSerializer::verify_signature(secure_serializer, signed_data, "public_key")
  assert_true(verification_result)
  
  // Test tampered data
  let tampered_data = signed_data.replace("critical", "tampered")
  let tampered_verification = SecureSerializer::verify_signature(secure_serializer, tampered_data, "public_key")
  assert_false(tampered_verification)
}

// Test 8: Cross-Format Serialization and Conversion
test "cross-format serialization and conversion" {
  // Create format converter
  let format_converter = FormatConverter::new()
  
  // Test JSON to XML conversion
  let json_data = "{\"name\":\"John\",\"age\":30,\"email\":\"john@example.com\"}"
  let xml_data = FormatConverter::json_to_xml(format_converter, json_data)
  
  assert_true(xml_data.contains("<name>John</name>"))
  assert_true(xml_data.contains("<age>30</age>"))
  assert_true(xml_data.contains("<email>john@example.com</email>"))
  
  // Test XML to JSON conversion
  let xml_input = "<person><name>Jane</name><age>25</age><city>New York</city></person>"
  let json_output = FormatConverter::xml_to_json(format_converter, xml_input)
  
  assert_true(json_output.contains("\"name\":\"Jane\""))
  assert_true(json_output.contains("\"age\":25"))
  assert_true(json_output.contains("\"city\":\"New York\""))
  
  // Test JSON to Protocol Buffers conversion
  let person_json = "{\"id\":123,\"name\":\"Bob\",\"status\":\"ACTIVE\"}"
  let protobuf_data = FormatConverter::json_to_protobuf(format_converter, person_json, "UserMessage")
  
  let back_to_json = FormatConverter::protobuf_to_json(format_converter, protobuf_data)
  assert_true(back_to_json.contains("\"id\":123"))
  assert_true(back_to_json.contains("\"name\":\"Bob\""))
  assert_true(back_to_json.contains("\"status\":\"ACTIVE\""))
  
  // Test CSV to JSON conversion
  let csv_input = "name,age,city\nJohn,30,New York\nJane,25,Boston\nBob,35,Chicago"
  let json_array = FormatConverter::csv_to_json(format_converter, csv_input)
  
  assert_true(json_array.contains("\"name\":\"John\""))
  assert_true(json_array.contains("\"age\":30"))
  assert_true(json_array.contains("\"city\":\"New York\""))
  
  // Test JSON to CSV conversion
  let json_array_input = "[{\"name\":\"Alice\",\"age\":28,\"city\":\"Seattle\"},{\"name\":\"Eve\",\"age\":32,\"city\":\"Portland\"}]"
  let csv_output = FormatConverter::json_to_csv(format_converter, json_array_input)
  
  assert_true(csv_output.contains("name,age,city"))
  assert_true(csv_output.contains("Alice,28,Seattle"))
  assert_true(csv_output.contains("Eve,32,Portland"))
  
  // Test YAML to JSON conversion
  let yaml_input = "
name: David
age: 40
address:
  street: 123 Main St
  city: Anytown
  country: USA
hobbies:
  - reading
  - swimming
  - coding
"
  
  let json_from_yaml = FormatConverter::yaml_to_json(format_converter, yaml_input)
  assert_true(json_from_yaml.contains("\"name\":\"David\""))
  assert_true(json_from_yaml.contains("\"age\":40"))
  assert_true(json_from_yaml.contains("\"street\":\"123 Main St\""))
  assert_true(json_from_yaml.contains("\"reading\""))
  
  // Test JSON to YAML conversion
  let json_for_yaml = "{\"name\":\"Eve\",\"age\":32,\"skills\":[\"JavaScript\",\"Python\",\"Go\"]}"
  let yaml_output = FormatConverter::json_to_yaml(format_converter, json_for_yaml)
  
  assert_true(yaml_output.contains("name: Eve"))
  assert_true(yaml_output.contains("age: 32"))
  assert_true(yaml_output.contains("- JavaScript"))
  assert_true(yaml_output.contains("- Python"))
  assert_true(yaml_output.contains("- Go"))
}

// Helper function to create large string
func create_large_string(size : Int) -> String {
  let result = ""
  let base_string = "abcdefghijklmnopqrstuvwxyz0123456789"
  
  while result.length() < size {
    result = result + base_string
  }
  
  result.substring(0, size)
}

// Helper function to create large test dataset
func create_large_test_dataset(count : Int) -> LargeDataset {
  let items = []
  
  for i in 0..count {
    let item = DatasetItem::new(
      "item_" + i.to_string(),
      i.to_float(),
      "description_" + i.to_string(),
      [i, i+1, i+2, i+3, i+5]
    )
    items.push(item)
  }
  
  LargeDataset::new(items)
}

// Helper function to create oversized data
func create_oversized_data(size : Int) -> OversizedData {
  let large_string = create_large_string(size)
  OversizedData::new(large_string)
}

// Helper function to create deeply nested data
func create_deeply_nested_data(depth : Int) -> NestedData {
  let base_data = BaseData::new("base")
  let current = NestedData::new("level_0", base_data)
  
  for i in 1..depth {
    current = NestedData::new("level_" + i.to_string(), BaseData::from_nested(current))
  }
  
  current
}