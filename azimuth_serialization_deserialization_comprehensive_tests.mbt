// Azimuth Telemetry System - Serialization and Deserialization Tests
// This file contains comprehensive test cases for serialization and deserialization

// Test 1: Basic JSON Serialization
test "basic json serialization" {
  // Test primitive types
  let int_value = 42
  let int_json = serialize_to_json(int_value)
  assert_eq(int_json, "42")
  
  let int_deserialized = deserialize_from_json(int_json)
  assert_eq(int_deserialized, int_value)
  
  let float_value = 3.14
  let float_json = serialize_to_json(float_value)
  assert_eq(float_json, "3.14")
  
  let float_deserialized = deserialize_from_json(float_json)
  assert_eq(float_deserialized, float_value)
  
  let string_value = "hello world"
  let string_json = serialize_to_json(string_value)
  assert_eq(string_json, "\"hello world\"")
  
  let string_deserialized = deserialize_from_json(string_json)
  assert_eq(string_deserialized, string_value)
  
  let bool_value = true
  let bool_json = serialize_to_json(bool_value)
  assert_eq(bool_json, "true")
  
  let bool_deserialized = deserialize_from_json(bool_json)
  assert_eq(bool_deserialized, bool_value)
  
  // Test null/none values
  let none_value = None
  let none_json = serialize_to_json(none_value)
  assert_eq(none_json, "null")
  
  let none_deserialized = deserialize_from_json(none_json)
  match none_deserialized {
    None => assert_true(true)
    Some(_) => assert_true(false)
  }
}

// Test 2: Complex JSON Serialization
test "complex json serialization" {
  // Test arrays
  let array_value = [1, 2, 3, 4, 5]
  let array_json = serialize_to_json(array_value)
  assert_eq(array_json, "[1,2,3,4,5]")
  
  let array_deserialized = deserialize_from_json(array_json)
  assert_eq(array_deserialized, array_value)
  
  // Test nested arrays
  let nested_array = [[1, 2], [3, 4], [5, 6]]
  let nested_array_json = serialize_to_json(nested_array)
  assert_eq(nested_array_json, "[[1,2],[3,4],[5,6]]")
  
  let nested_array_deserialized = deserialize_from_json(nested_array_json)
  assert_eq(nested_array_deserialized, nested_array)
  
  // Test objects/records
  let object_value = {
    "name": "John Doe",
    "age": 30,
    "email": "john@example.com",
    "active": true
  }
  let object_json = serialize_to_json(object_value)
  assert_true(object_json.contains("\"name\":\"John Doe\""))
  assert_true(object_json.contains("\"age\":30"))
  assert_true(object_json.contains("\"email\":\"john@example.com\""))
  assert_true(object_json.contains("\"active\":true"))
  
  let object_deserialized = deserialize_from_json(object_json)
  assert_eq(object_deserialized.name, "John Doe")
  assert_eq(object_deserialized.age, 30)
  assert_eq(object_deserialized.email, "john@example.com")
  assert_eq(object_deserialized.active, true)
  
  // Test nested objects
  let nested_object = {
    "user": {
      "name": "Jane Doe",
      "contact": {
        "email": "jane@example.com",
        "phone": "123-456-7890"
      }
    },
    "settings": {
      "theme": "dark",
      "notifications": true
    }
  }
  let nested_object_json = serialize_to_json(nested_object)
  let nested_object_deserialized = deserialize_from_json(nested_object_json)
  
  assert_eq(nested_object_deserialized.user.name, "Jane Doe")
  assert_eq(nested_object_deserialized.user.contact.email, "jane@example.com")
  assert_eq(nested_object_deserialized.settings.theme, "dark")
  assert_eq(nested_object_deserialized.settings.notifications, true)
}

// Test 3: Binary Serialization
test "binary serialization" {
  // Test primitive types
  let int_value = 42
  let int_binary = serialize_to_binary(int_value)
  let int_deserialized = deserialize_from_binary(int_binary)
  assert_eq(int_deserialized, int_value)
  
  let float_value = 3.14
  let float_binary = serialize_to_binary(float_value)
  let float_deserialized = deserialize_from_binary(float_binary)
  assert_eq(float_deserialized, float_value)
  
  let string_value = "hello world"
  let string_binary = serialize_to_binary(string_value)
  let string_deserialized = deserialize_from_binary(string_binary)
  assert_eq(string_deserialized, string_value)
  
  // Test arrays
  let array_value = [1, 2, 3, 4, 5]
  let array_binary = serialize_to_binary(array_value)
  let array_deserialized = deserialize_from_binary(array_binary)
  assert_eq(array_deserialized, array_value)
  
  // Test objects
  let object_value = {
    "name": "John Doe",
    "age": 30,
    "active": true
  }
  let object_binary = serialize_to_binary(object_value)
  let object_deserialized = deserialize_from_binary(object_binary)
  assert_eq(object_deserialized.name, "John Doe")
  assert_eq(object_deserialized.age, 30)
  assert_eq(object_deserialized.active, true)
  
  // Test binary serialization is more compact than JSON
  let object_json = serialize_to_json(object_value)
  assert_true(int_binary.length() < int_json.length())
  assert_true(object_binary.length() < object_json.length())
}

// Test 4: Protocol Buffers Serialization
test "protocol buffers serialization" {
  // Test with Protocol Buffers
  let proto_message = {
    "trace_id": "0af7651916cd43dd8448eb211c80319c",
    "span_id": "b7ad6b7169203331",
    "parent_span_id": "b7ad6b7169203330",
    "operation_name": "http.request",
    "start_time": 1589273771000000000,
    "end_time": 1589273772000000000,
    "status": 1,
    "attributes": [
      {"key": "http.method", "value": {"string_value": "GET"}},
      {"key": "http.url", "value": {"string_value": "https://example.com/api"}},
      {"key": "http.status_code", "value": {"int_value": 200}}
    ]
  }
  
  // Serialize to Protocol Buffers
  let proto_binary = serialize_to_protobuf(proto_message)
  
  // Deserialize from Protocol Buffers
  let proto_deserialized = deserialize_from_protobuf(proto_binary)
  
  // Verify deserialized data
  assert_eq(proto_deserialized.trace_id, "0af7651916cd43dd8448eb211c80319c")
  assert_eq(proto_deserialized.span_id, "b7ad6b7169203331")
  assert_eq(proto_deserialized.operation_name, "http.request")
  assert_eq(proto_deserialized.status, 1)
  assert_eq(proto_deserialized.attributes.length(), 3)
  
  // Test Protocol Buffers is more compact than JSON
  let proto_json = serialize_to_json(proto_message)
  assert_true(proto_binary.length() < proto_json.length())
}

// Test 5: MessagePack Serialization
test "messagepack serialization" {
  // Test with MessagePack
  let msgpack_data = {
    "service": "payment-service",
    "version": "1.2.3",
    "instances": 3,
    "healthy": true,
    "metadata": {
      "region": "us-west-2",
      "zone": "us-west-2a",
      "tags": ["payment", "critical", "v1"]
    },
    "metrics": [
      {"name": "request_count", "value": 12345, "type": "counter"},
      {"name": "response_time", "value": 123.45, "type": "histogram"},
      {"name": "error_rate", "value": 0.01, "type": "gauge"}
    ]
  }
  
  // Serialize to MessagePack
  let msgpack_binary = serialize_to_msgpack(msgpack_data)
  
  // Deserialize from MessagePack
  let msgpack_deserialized = deserialize_from_msgpack(msgpack_binary)
  
  // Verify deserialized data
  assert_eq(msgpack_deserialized.service, "payment-service")
  assert_eq(msgpack_deserialized.version, "1.2.3")
  assert_eq(msgpack_deserialized.instances, 3)
  assert_eq(msgpack_deserialized.healthy, true)
  assert_eq(msgpack_deserialized.metadata.region, "us-west-2")
  assert_eq(msgpack_deserialized.metadata.tags.length(), 3)
  assert_eq(msgpack_deserialized.metrics.length(), 3)
  assert_eq(msgpack_deserialized.metrics[0].name, "request_count")
  assert_eq(msgpack_deserialized.metrics[0].value, 12345)
  
  // Test MessagePack is more compact than JSON
  let msgpack_json = serialize_to_json(msgpack_data)
  assert_true(msgpack_binary.length() < msgpack_json.length())
}

// Test 6: XML Serialization
test "xml serialization" {
  // Test with XML
  let xml_data = {
    "order": {
      "id": "12345",
      "customer": {
        "name": "John Doe",
        "email": "john@example.com",
        "address": {
          "street": "123 Main St",
          "city": "Anytown",
          "state": "CA",
          "zip": "12345"
        }
      },
      "items": [
        {"id": "1", "name": "Product A", "quantity": 2, "price": 19.99},
        {"id": "2", "name": "Product B", "quantity": 1, "price": 29.99}
      ],
      "total": 69.97
    }
  }
  
  // Serialize to XML
  let xml_string = serialize_to_xml(xml_data)
  
  // Verify XML structure
  assert_true(xml_string.contains("<order>"))
  assert_true(xml_string.contains("<id>12345</id>"))
  assert_true(xml_string.contains("<customer>"))
  assert_true(xml_string.contains("<name>John Doe</name>"))
  assert_true(xml_string.contains("<items>"))
  assert_true(xml_string.contains("<item>"))
  assert_true(xml_string.contains("<name>Product A</name>"))
  assert_true(xml_string.contains("<quantity>2</quantity>"))
  assert_true(xml_string.contains("<price>19.99</price>"))
  assert_true(xml_string.contains("<total>69.97</total>"))
  
  // Deserialize from XML
  let xml_deserialized = deserialize_from_xml(xml_string)
  
  // Verify deserialized data
  assert_eq(xml_deserialized.order.id, "12345")
  assert_eq(xml_deserialized.order.customer.name, "John Doe")
  assert_eq(xml_deserialized.order.customer.address.city, "Anytown")
  assert_eq(xml_deserialized.order.items.length(), 2)
  assert_eq(xml_deserialized.order.items[0].name, "Product A")
  assert_eq(xml_deserialized.order.items[0].quantity, 2)
  assert_eq(xml_deserialized.order.total, 69.97)
}

// Test 7: Custom Serialization Formats
test "custom serialization formats" {
  // Test with custom format (e.g., CSV)
  let csv_data = [
    {"name": "John", "age": 30, "city": "New York"},
    {"name": "Jane", "age": 25, "city": "Los Angeles"},
    {"name": "Bob", "age": 35, "city": "Chicago"}
  ]
  
  // Serialize to CSV
  let csv_string = serialize_to_csv(csv_data)
  
  // Verify CSV structure
  assert_true(csv_string.contains("name,age,city"))
  assert_true(csv_string.contains("John,30,New York"))
  assert_true(csv_string.contains("Jane,25,Los Angeles"))
  assert_true(csv_string.contains("Bob,35,Chicago"))
  
  // Deserialize from CSV
  let csv_deserialized = deserialize_from_csv(csv_string)
  
  // Verify deserialized data
  assert_eq(csv_deserialized.length(), 3)
  assert_eq(csv_deserialized[0].name, "John")
  assert_eq(csv_deserialized[0].age, 30)
  assert_eq(csv_deserialized[0].city, "New York")
  assert_eq(csv_deserialized[1].name, "Jane")
  assert_eq(csv_deserialized[1].age, 25)
  assert_eq(csv_deserialized[1].city, "Los Angeles")
  
  // Test with custom format (e.g., YAML)
  let yaml_data = {
    "application": {
      "name": "my-app",
      "version": "1.0.0",
      "environment": "production",
      "servers": [
        {"host": "server1.example.com", "port": 8080, "weight": 1},
        {"host": "server2.example.com", "port": 8080, "weight": 2}
      ],
      "database": {
        "host": "db.example.com",
        "port": 5432,
        "name": "myapp_prod",
        "ssl": true
      }
    }
  }
  
  // Serialize to YAML
  let yaml_string = serialize_to_yaml(yaml_data)
  
  // Verify YAML structure
  assert_true(yaml_string.contains("name: my-app"))
  assert_true(yaml_string.contains("version: 1.0.0"))
  assert_true(yaml_string.contains("environment: production"))
  assert_true(yaml_string.contains("servers:"))
  assert_true(yaml_string.contains("- host: server1.example.com"))
  assert_true(yaml_string.contains("port: 8080"))
  assert_true(yaml_string.contains("database:"))
  assert_true(yaml_string.contains("ssl: true"))
  
  // Deserialize from YAML
  let yaml_deserialized = deserialize_from_yaml(yaml_string)
  
  // Verify deserialized data
  assert_eq(yaml_deserialized.application.name, "my-app")
  assert_eq(yaml_deserialized.application.version, "1.0.0")
  assert_eq(yaml_deserialized.application.environment, "production")
  assert_eq(yaml_deserialized.application.servers.length(), 2)
  assert_eq(yaml_deserialized.application.servers[0].host, "server1.example.com")
  assert_eq(yaml_deserialized.application.database.ssl, true)
}

// Test 8: Telemetry Data Serialization
test "telemetry data serialization" {
  // Test span serialization
  let span = {
    "trace_id": "0af7651916cd43dd8448eb211c80319c",
    "span_id": "b7ad6b7169203331",
    "parent_span_id": "b7ad6b7169203330",
    "operation_name": "http.request",
    "kind": "SERVER",
    "start_time": 1589273771000000000,
    "end_time": 1589273772000000000,
    "status": {
      "code": 1,
      "message": "OK"
    },
    "attributes": [
      {"key": "http.method", "value": {"string_value": "GET"}},
      {"key": "http.url", "value": {"string_value": "https://example.com/api"}},
      {"key": "http.status_code", "value": {"int_value": 200}}
    ],
    "events": [
      {
        "name": "exception",
        "timestamp": 1589273771500000000,
        "attributes": [
          {"key": "exception.type", "value": {"string_value": "TimeoutException"}},
          {"key": "exception.message", "value": {"string_value": "Request timed out"}}
        ]
      }
    ],
    "links": []
  }
  
  // Serialize span to different formats
  let span_json = serialize_to_json(span)
  let span_binary = serialize_to_binary(span)
  let span_protobuf = serialize_to_protobuf(span)
  
  // Deserialize and verify
  let span_from_json = deserialize_from_json(span_json)
  let span_from_binary = deserialize_from_binary(span_binary)
  let span_from_protobuf = deserialize_from_protobuf(span_protobuf)
  
  assert_eq(span_from_json.operation_name, "http.request")
  assert_eq(span_from_json.attributes.length(), 3)
  assert_eq(span_from_json.events.length(), 1)
  
  assert_eq(span_from_binary.operation_name, "http.request")
  assert_eq(span_from_binary.attributes.length(), 3)
  assert_eq(span_from_binary.events.length(), 1)
  
  assert_eq(span_from_protobuf.operation_name, "http.request")
  assert_eq(span_from_protobuf.attributes.length(), 3)
  assert_eq(span_from_protobuf.events.length(), 1)
  
  // Test metric serialization
  let metric = {
    "name": "http.requests",
    "description": "Total number of HTTP requests",
    "unit": "count",
    "data": {
      "type": "sum",
      "is_monotonic": true,
      "aggregation_temporality": 1,
      "data_points": [
        {
          "start_time_unix_nano": 1589273770000000000,
          "time_unix_nano": 1589273771000000000,
          "value": 12345,
          "attributes": [
            {"key": "http.method", "value": {"string_value": "GET"}},
            {"key": "http.status_code", "value": {"int_value": 200}}
          ]
        }
      ]
    }
  }
  
  // Serialize metric to different formats
  let metric_json = serialize_to_json(metric)
  let metric_binary = serialize_to_binary(metric)
  
  // Deserialize and verify
  let metric_from_json = deserialize_from_json(metric_json)
  let metric_from_binary = deserialize_from_binary(metric_binary)
  
  assert_eq(metric_from_json.name, "http.requests")
  assert_eq(metric_from_json.data.type, "sum")
  assert_eq(metric_from_json.data.data_points.length(), 1)
  assert_eq(metric_from_json.data.data_points[0].value, 12345)
  
  assert_eq(metric_from_binary.name, "http.requests")
  assert_eq(metric_from_binary.data.type, "sum")
  assert_eq(metric_from_binary.data.data_points.length(), 1)
  assert_eq(metric_from_binary.data.data_points[0].value, 12345)
  
  // Test log serialization
  let log = {
    "time_unix_nano": 1589273771000000000,
    "observed_time_unix_nano": 1589273771000000000,
    "severity_number": 9,
    "severity_text": "INFO",
    "body": {
      "string_value": "User login successful"
    },
    "attributes": [
      {"key": "user.id", "value": {"string_value": "12345"}},
      {"key": "user.name", "value": {"string_value": "john.doe"}},
      {"key": "ip.address", "value": {"string_value": "192.168.1.100"}}
    ],
    "trace_id": "0af7651916cd43dd8448eb211c80319c",
    "span_id": "b7ad6b7169203331",
    "flags": 1
  }
  
  // Serialize log to different formats
  let log_json = serialize_to_json(log)
  let log_binary = serialize_to_binary(log)
  
  // Deserialize and verify
  let log_from_json = deserialize_from_json(log_json)
  let log_from_binary = deserialize_from_binary(log_binary)
  
  assert_eq(log_from_json.severity_text, "INFO")
  assert_eq(log_from_json.body.string_value, "User login successful")
  assert_eq(log_from_json.attributes.length(), 3)
  assert_eq(log_from_json.trace_id, "0af7651916cd43dd8448eb211c80319c")
  
  assert_eq(log_from_binary.severity_text, "INFO")
  assert_eq(log_from_binary.body.string_value, "User login successful")
  assert_eq(log_from_binary.attributes.length(), 3)
  assert_eq(log_from_binary.trace_id, "0af7651916cd43dd8448eb211c80319c")
}

// Test 9: Serialization Performance Comparison
test "serialization performance comparison" {
  // Create large test data
  let large_data = {
    "spans": [],
    "metrics": [],
    "logs": []
  }
  
  // Generate 1000 spans
  for i in 0..1000 {
    large_data.spans.push({
      "trace_id": "trace_" + i.to_string(),
      "span_id": "span_" + i.to_string(),
      "operation_name": "operation_" + i.to_string(),
      "start_time": 1589273771000000000 + i,
      "end_time": 1589273772000000000 + i,
      "attributes": [
        {"key": "index", "value": {"int_value": i}},
        {"key": "name", "value": {"string_value": "item_" + i.to_string()}}
      ]
    })
  }
  
  // Generate 1000 metrics
  for i in 0..1000 {
    large_data.metrics.push({
      "name": "metric_" + i.to_string(),
      "value": i * 1.5,
      "attributes": [
        {"key": "index", "value": {"int_value": i}},
        {"key": "type", "value": {"string_value": "gauge"}}
      ]
    })
  }
  
  // Generate 1000 logs
  for i in 0..1000 {
    large_data.logs.push({
      "time": 1589273771000000000 + i,
      "severity": "INFO",
      "message": "Log message " + i.to_string(),
      "attributes": [
        {"key": "index", "value": {"int_value": i}},
        {"key": "source", "value": {"string_value": "component_" + (i % 10).to_string()}}
      ]
    })
  }
  
  // Benchmark JSON serialization
  let json_serialize_time = benchmark(|| {
    serialize_to_json(large_data)
  })
  
  // Benchmark JSON deserialization
  let json_data = serialize_to_json(large_data)
  let json_deserialize_time = benchmark(|| {
    deserialize_from_json(json_data)
  })
  
  // Benchmark binary serialization
  let binary_serialize_time = benchmark(|| {
    serialize_to_binary(large_data)
  })
  
  // Benchmark binary deserialization
  let binary_data = serialize_to_binary(large_data)
  let binary_deserialize_time = benchmark(|| {
    deserialize_from_binary(binary_data)
  })
  
  // Benchmark Protocol Buffers serialization
  let protobuf_serialize_time = benchmark(|| {
    serialize_to_protobuf(large_data)
  })
  
  // Benchmark Protocol Buffers deserialization
  let protobuf_data = serialize_to_protobuf(large_data)
  let protobuf_deserialize_time = benchmark(|| {
    deserialize_from_protobuf(protobuf_data)
  })
  
  // Verify size differences
  let json_size = json_data.length()
  let binary_size = binary_data.length()
  let protobuf_size = protobuf_data.length()
  
  // Binary formats should be more compact
  assert_true(binary_size < json_size)
  assert_true(protobuf_size < json_size)
  
  // Binary formats should be faster for serialization
  assert_true(binary_serialize_time < json_serialize_time)
  assert_true(protobuf_serialize_time < json_serialize_time)
  
  // Binary formats should be faster for deserialization
  assert_true(binary_deserialize_time < json_deserialize_time)
  assert_true(protobuf_deserialize_time < json_deserialize_time)
}

// Test 10: Serialization Error Handling
test "serialization error handling" {
  // Test invalid JSON deserialization
  let invalid_json = "{invalid json}"
  let result = try {
    deserialize_from_json(invalid_json)
  } catch {
    SerializationError(message) => {
      assert_true(message.contains("Invalid JSON"))
      "error_caught"
    }
    _ => "unexpected_error"
  }
  
  assert_eq(result, "error_caught")
  
  // Test invalid binary deserialization
  let invalid_binary = [0xFF, 0xFF, 0xFF, 0xFF]
  let result2 = try {
    deserialize_from_binary(invalid_binary)
  } catch {
    SerializationError(message) => {
      assert_true(message.contains("Invalid binary data"))
      "error_caught"
    }
    _ => "unexpected_error"
  }
  
  assert_eq(result2, "error_caught")
  
  // Test circular reference handling
  let circular_object = {}
  circular_object.self = circular_object  // Create circular reference
  
  let result3 = try {
    serialize_to_json(circular_object)
  } catch {
    SerializationError(message) => {
      assert_true(message.contains("Circular reference"))
      "error_caught"
    }
    _ => "unexpected_error"
  }
  
  assert_eq(result3, "error_caught")
  
  // Test too large data handling
  let large_string = "a" * 1000000  // 1MB string
  let large_object = {
    "data": large_string
  }
  
  // This should succeed
  let large_json = serialize_to_json(large_object)
  let large_deserialized = deserialize_from_json(large_json)
  assert_eq(large_deserialized.data, large_string)
  
  // Create extremely large object (should fail)
  let extremely_large_object = {
    "data": "a" * 100000000  // 100MB string
  }
  
  let result4 = try {
    serialize_to_json(extremely_large_object)
  } catch {
    SerializationError(message) => {
      assert_true(message.contains("Data too large"))
      "error_caught"
    }
    _ => "unexpected_error"
  }
  
  assert_eq(result4, "error_caught")
  
  // Test type mismatch handling
  let json_string = "\"hello\""  // String, but we expect a number
  let result5 = try {
    deserialize_from_json_typed[Int](json_string)
  } catch {
    SerializationError(message) => {
      assert_true(message.contains("Type mismatch"))
      "error_caught"
    }
    _ => "unexpected_error"
  }
  
  assert_eq(result5, "error_caught")
  
  // Test missing field handling
  let json_object = "{\"name\": \"John\"}"  // Missing "age" field
  let result6 = try {
    deserialize_from_json_typed[Person](json_object)
  } catch {
    SerializationError(message) => {
      assert_true(message.contains("Missing field"))
      "error_caught"
    }
    _ => "unexpected_error"
  }
  
  assert_eq(result6, "error_caught")
}