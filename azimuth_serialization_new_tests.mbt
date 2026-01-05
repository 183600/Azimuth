// Azimuth Telemetry System - Serialization Tests
// This file contains test cases for serialization and deserialization functionality

// Test 1: JSON Serialization
test "json serialization" {
  let json_serializer = JsonSerializer::new()
  
  // Test basic types
  let string_value = "test_string"
  let int_value = 42
  let float_value = 3.14
  let bool_value = true
  
  let serialized_string = JsonSerializer::serialize(json_serializer, string_value)
  let serialized_int = JsonSerializer::serialize(json_serializer, int_value)
  let serialized_float = JsonSerializer::serialize(json_serializer, float_value)
  let serialized_bool = JsonSerializer::serialize(json_serializer, bool_value)
  
  assert_eq(serialized_string, "\"test_string\"")
  assert_eq(serialized_int, "42")
  assert_eq(serialized_float, "3.14")
  assert_eq(serialized_bool, "true")
  
  // Test arrays and objects
  let array_value = [1, 2, 3, 4, 5]
  let object_value = {"key1": "value1", "key2": 42, "key3": true}
  
  let serialized_array = JsonSerializer::serialize(json_serializer, array_value)
  let serialized_object = JsonSerializer::serialize(json_serializer, object_value)
  
  assert_true(serialized_array.contains("[1,2,3,4,5]"))
  assert_true(serialized_object.contains("key1"))
  assert_true(serialized_object.contains("value1"))
  
  // Test complex nested structure
  let complex_value = {
    "name": "test_object",
    "id": 123,
    "active": true,
    "tags": ["tag1", "tag2", "tag3"],
    "metadata": {
      "created": "2023-01-01",
      "version": 1.0
    }
  }
  
  let serialized_complex = JsonSerializer::serialize(json_serializer, complex_value)
  assert_true(serialized_complex.contains("test_object"))
  assert_true(serialized_complex.contains("tag1"))
  assert_true(serialized_complex.contains("2023-01-01"))
}

// Test 2: JSON Deserialization
test "json deserialization" {
  let json_serializer = JsonSerializer::new()
  
  // Test basic types
  let string_json = "\"test_string\""
  let int_json = "42"
  let float_json = "3.14"
  let bool_json = "true"
  
  let deserialized_string = JsonSerializer::deserialize_string(json_serializer, string_json)
  let deserialized_int = JsonSerializer::deserialize_int(json_serializer, int_json)
  let deserialized_float = JsonSerializer::deserialize_float(json_serializer, float_json)
  let deserialized_bool = JsonSerializer::deserialize_bool(json_serializer, bool_json)
  
  match deserialized_string {
    Ok(value) => assert_eq(value, "test_string")
    Err(_) => assert_true(false)
  }
  
  match deserialized_int {
    Ok(value) => assert_eq(value, 42)
    Err(_) => assert_true(false)
  }
  
  match deserialized_float {
    Ok(value) => assert_true(value - 3.14 < 0.001)
    Err(_) => assert_true(false)
  }
  
  match deserialized_bool {
    Ok(value) => assert_true(value)
    Err(_) => assert_true(false)
  }
  
  // Test arrays and objects
  let array_json = "[1,2,3,4,5]"
  let object_json = "{\"key1\":\"value1\",\"key2\":42,\"key3\":true}"
  
  let deserialized_array = JsonSerializer::deserialize_array(json_serializer, array_json)
  let deserialized_object = JsonSerializer::deserialize_object(json_serializer, object_json)
  
  match deserialized_array {
    Ok(array) => assert_eq(array.length(), 5)
    Err(_) => assert_true(false)
  }
  
  match deserialized_object {
    Ok(object) => {
      match object.get("key1") {
        Some(value) => assert_eq(value, "value1")
        None => assert_true(false)
      }
    }
    Err(_) => assert_true(false)
  }
}

// Test 3: Protocol Buffers Serialization
test "protobuf serialization" {
  let protobuf_serializer = ProtobufSerializer::new()
  
  // Create test message
  let test_message = TestMessage::new()
  TestMessage::set_id(test_message, 123)
  TestMessage::set_name(test_message, "test_message")
  TestMessage::set_active(test_message, true)
  TestMessage::add_tags(test_message, "tag1")
  TestMessage::add_tags(test_message, "tag2")
  
  // Serialize to bytes
  let serialized_bytes = ProtobufSerializer::serialize(protobuf_serializer, test_message)
  assert_true(serialized_bytes.length() > 0)
  
  // Deserialize from bytes
  let deserialized_message = ProtobufSerializer::deserialize(protobuf_serializer, serialized_bytes)
  
  match deserialized_message {
    Ok(message) => {
      assert_eq(TestMessage::id(message), 123)
      assert_eq(TestMessage::name(message), "test_message")
      assert_true(TestMessage::active(message))
      assert_eq(TestMessage::tags(message).length(), 2)
    }
    Err(_) => assert_true(false)
  }
}

// Test 4: MessagePack Serialization
test "msgpack serialization" {
  let msgpack_serializer = MessagePackSerializer::new()
  
  // Test basic types
  let test_data = {
    "string": "test_value",
    "integer": 42,
    "float": 3.14,
    "boolean": true,
    "array": [1, 2, 3],
    "null": null
  }
  
  // Serialize to bytes
  let serialized_bytes = MessagePackSerializer::serialize(msgpack_serializer, test_data)
  assert_true(serialized_bytes.length() > 0)
  
  // Deserialize from bytes
  let deserialized_data = MessagePackSerializer::deserialize(msgpack_serializer, serialized_bytes)
  
  match deserialized_data {
    Ok(data) => {
      match data.get("string") {
        Some(value) => assert_eq(value, "test_value")
        None => assert_true(false)
      }
      
      match data.get("integer") {
        Some(value) => assert_eq(value, 42)
        None => assert_true(false)
      }
      
      match data.get("float") {
        Some(value) => assert_true(value - 3.14 < 0.001)
        None => assert_true(false)
      }
      
      match data.get("boolean") {
        Some(value) => assert_true(value)
        None => assert_true(false)
      }
    }
    Err(_) => assert_true(false)
  }
}

// Test 5: XML Serialization
test "xml serialization" {
  let xml_serializer = XmlSerializer::new()
  
  // Create test object
  let test_object = {
    "name": "test_object",
    "id": 123,
    "active": true,
    "attributes": {
      "color": "red",
      "size": "large"
    },
    "children": [
      {"name": "child1", "value": 10},
      {"name": "child2", "value": 20}
    ]
  }
  
  // Serialize to XML
  let xml_string = XmlSerializer::serialize(xml_serializer, test_object, "root")
  assert_true(xml_string.length() > 0)
  assert_true(xml_string.contains("<root>"))
  assert_true(xml_string.contains("</root>"))
  assert_true(xml_string.contains("<name>test_object</name>"))
  assert_true(xml_string.contains("<id>123</id>"))
  assert_true(xml_string.contains("<active>true</active>"))
  assert_true(xml_string.contains("<children>"))
  
  // Deserialize from XML
  let deserialized_object = XmlSerializer::deserialize(xml_serializer, xml_string)
  
  match deserialized_object {
    Ok(obj) => {
      match obj.get("name") {
        Some(value) => assert_eq(value, "test_object")
        None => assert_true(false)
      }
      
      match obj.get("id") {
        Some(value) => assert_eq(value, 123)
        None => assert_true(false)
      }
    }
    Err(_) => assert_true(false)
  }
}

// Test 6: CSV Serialization
test "csv serialization" {
  let csv_serializer = CsvSerializer::new()
  
  // Create test records
  let test_records = [
    {"name": "Alice", "age": 30, "city": "New York"},
    {"name": "Bob", "age": 25, "city": "Los Angeles"},
    {"name": "Charlie", "age": 35, "city": "Chicago"}
  ]
  
  // Serialize to CSV
  let csv_string = CsvSerializer::serialize(csv_serializer, test_records)
  assert_true(csv_string.length() > 0)
  assert_true(csv_string.contains("name,age,city"))
  assert_true(csv_string.contains("Alice,30,New York"))
  assert_true(csv_string.contains("Bob,25,Los Angeles"))
  assert_true(csv_string.contains("Charlie,35,Chicago"))
  
  // Deserialize from CSV
  let deserialized_records = CsvSerializer::deserialize(csv_serializer, csv_string)
  
  match deserialized_records {
    Ok(records) => {
      assert_eq(records.length(), 3)
      
      let first_record = records[0]
      match first_record.get("name") {
        Some(value) => assert_eq(value, "Alice")
        None => assert_true(false)
      }
      
      match first_record.get("age") {
        Some(value) => assert_eq(value, "30")
        None => assert_true(false)
      }
    }
    Err(_) => assert_true(false)
  }
}

// Test 7: YAML Serialization
test "yaml serialization" {
  let yaml_serializer = YamlSerializer::new()
  
  // Create test object
  let test_object = {
    "name": "test_config",
    "version": "1.0.0",
    "settings": {
      "debug": true,
      "timeout": 30,
      "servers": ["server1", "server2", "server3"]
    },
    "features": {
      "feature1": {
        "enabled": true,
        "options": {
          "option1": "value1",
          "option2": 42
        }
      },
      "feature2": {
        "enabled": false
      }
    }
  }
  
  // Serialize to YAML
  let yaml_string = YamlSerializer::serialize(yaml_serializer, test_object)
  assert_true(yaml_string.length() > 0)
  assert_true(yaml_string.contains("name: test_config"))
  assert_true(yaml_string.contains("version: 1.0.0"))
  assert_true(yaml_string.contains("debug: true"))
  assert_true(yaml_string.contains("servers:"))
  assert_true(yaml_string.contains("- server1"))
  
  // Deserialize from YAML
  let deserialized_object = YamlSerializer::deserialize(yaml_serializer, yaml_string)
  
  match deserialized_object {
    Ok(obj) => {
      match obj.get("name") {
        Some(value) => assert_eq(value, "test_config")
        None => assert_true(false)
      }
      
      match obj.get("version") {
        Some(value) => assert_eq(value, "1.0.0")
        None => assert_true(false)
      }
    }
    Err(_) => assert_true(false)
  }
}

// Test 8: Custom Serialization Format
test "custom serialization format" {
  let custom_serializer = CustomFormatSerializer::new()
  
  // Define custom format rules
  CustomFormatSerializer::add_rule(custom_serializer, "prefix", "key:", "value;")
  CustomFormatSerializer::add_rule(custom_serializer, "separator", "|")
  CustomFormatSerializer::add_rule(custom_serializer, "terminator", "#")
  
  // Create test data
  let test_data = {
    "key1": "value1",
    "key2": "value2",
    "key3": "value3"
  }
  
  // Serialize to custom format
  let custom_string = CustomFormatSerializer::serialize(custom_serializer, test_data)
  assert_true(custom_string.length() > 0)
  assert_true(custom_string.contains("key:value1;"))
  assert_true(custom_string.contains("key:value2;"))
  assert_true(custom_string.contains("key:value3;"))
  assert_true(custom_string.contains("|"))
  assert_true(custom_string.contains("#"))
  
  // Deserialize from custom format
  let deserialized_data = CustomFormatSerializer::deserialize(custom_serializer, custom_string)
  
  match deserialized_data {
    Ok(data) => {
      match data.get("key1") {
        Some(value) => assert_eq(value, "value1")
        None => assert_true(false)
      }
      
      match data.get("key2") {
        Some(value) => assert_eq(value, "value2")
        None => assert_true(false)
      }
    }
    Err(_) => assert_true(false)
  }
}

// Test 9: Serialization with Compression
test "serialization with compression" {
  let json_serializer = JsonSerializer::new()
  let compressor = GzipCompressor::new()
  
  // Create large test data
  let large_data = []
  for i in 0..<1000 {
    large_data.push({
      "id": i,
      "name": "item_" + i.to_string(),
      "description": "This is a detailed description for item " + i.to_string() + " with a lot of text to make it larger",
      "tags": ["tag1", "tag2", "tag3", "tag4", "tag5"],
      "metadata": {
        "created": "2023-01-01",
        "updated": "2023-01-02",
        "version": 1.0,
        "active": true
      }
    })
  }
  
  // Serialize to JSON
  let json_string = JsonSerializer::serialize(json_serializer, large_data)
  let original_size = json_string.length()
  
  // Compress serialized data
  let compressed_data = GzipCompressor::compress(compressor, json_string)
  let compressed_size = compressed_data.length()
  
  // Verify compression reduced size
  assert_true(compressed_size < original_size)
  
  // Decompress data
  let decompressed_string = GzipCompressor::decompress(compressor, compressed_data)
  
  // Verify decompressed data matches original
  assert_eq(decompressed_string, json_string)
  
  // Deserialize decompressed data
  let deserialized_data = JsonSerializer::deserialize_array(json_serializer, decompressed_string)
  
  match deserialized_data {
    Ok(data) => assert_eq(data.length(), 1000)
    Err(_) => assert_true(false)
  }
}

// Test 10: Serialization Error Handling
test "serialization error handling" {
  let json_serializer = JsonSerializer::new()
  
  // Test invalid JSON
  let invalid_json = "{\"invalid\": json}"
  let deserialization_result = JsonSerializer::deserialize_object(json_serializer, invalid_json)
  
  match deserialization_result {
    Ok(_) => assert_true(false) // Should fail
    Err(error) => assert_true(error.contains("Invalid JSON"))
  }
  
  // Test circular reference detection
  let circular_object = {"name": "circular"}
  circular_object["self"] = circular_object
  
  let circular_serialization_result = JsonSerializer::serialize(json_serializer, circular_object)
  match circular_serialization_result {
    Ok(_) => assert_true(false) // Should fail
    Err(error) => assert_true(error.contains("Circular reference"))
  }
  
  // Test type mismatch handling
  let type_mismatch_json = "{\"number\": \"not_a_number\"}"
  let type_deserialization_result = JsonSerializer::deserialize_int(json_serializer, type_mismatch_json)
  
  match type_deserialization_result {
    Ok(_) => assert_true(false) // Should fail
    Err(error) => assert_true(error.contains("Type mismatch"))
  }
  
  // Test missing required field handling
  let incomplete_json = "{\"name\": \"test\"}" // Missing required 'id' field
  let incomplete_deserialization_result = JsonSerializer::deserialize_struct(json_serializer, incomplete_json, "TestStruct")
  
  match incomplete_deserialization_result {
    Ok(_) => assert_true(false) // Should fail
    Err(error) => assert_true(error.contains("Missing required field"))
  }
  
  // Test oversized data handling
  let oversized_data = "x" * 1000000 // 1MB string
  let oversized_serialization_result = JsonSerializer::serialize(json_serializer, oversized_data)
  
  match oversized_serialization_result {
    Ok(_) => assert_true(false) // Should fail
    Err(error) => assert_true(error.contains("Data too large"))
  }
}