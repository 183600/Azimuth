// Azimuth Telemetry System - Serialization and Deserialization Tests
// This file contains comprehensive test cases for serialization and deserialization functionality

// Test 1: Basic JSON Serialization
test "basic json serialization" {
  let serializer = JsonSerializer::new()
  
  // Test primitive type serialization
  let int_value = 42
  let serialized_int = JsonSerializer::serialize(serializer, int_value)
  assert_eq(serialized_int, "42")
  
  let float_value = 3.14
  let serialized_float = JsonSerializer::serialize(serializer, float_value)
  assert_eq(serialized_float, "3.14")
  
  let string_value = "hello world"
  let serialized_string = JsonSerializer::serialize(serializer, string_value)
  assert_eq(serialized_string, "\"hello world\"")
  
  let bool_value = true
  let serialized_bool = JsonSerializer::serialize(serializer, bool_value)
  assert_eq(serialized_bool, "true")
  
  // Test array serialization
  let array_value = [1, 2, 3, 4, 5]
  let serialized_array = JsonSerializer::serialize(serializer, array_value)
  assert_eq(serialized_array, "[1,2,3,4,5]")
  
  // Test object serialization
  let object_value = {
    name: "test",
    age: 25,
    active: true
  }
  let serialized_object = JsonSerializer::serialize(serializer, object_value)
  assert_eq(serialized_object, "{\"name\":\"test\",\"age\":25,\"active\":true}")
}

// Test 2: Basic JSON Deserialization
test "basic json deserialization" {
  let deserializer = JsonDeserializer::new()
  
  // Test primitive type deserialization
  let int_json = "42"
  let deserialized_int = JsonDeserializer::deserialize_int(deserializer, int_json)
  match deserialized_int {
    Ok(value) => assert_eq(value, 42)
    Err(_) => assert_true(false)
  }
  
  let float_json = "3.14"
  let deserialized_float = JsonDeserializer::deserialize_float(deserializer, float_json)
  match deserialized_float {
    Ok(value) => assert_eq(value, 3.14)
    Err(_) => assert_true(false)
  }
  
  let string_json = "\"hello world\""
  let deserialized_string = JsonDeserializer::deserialize_string(deserializer, string_json)
  match deserialized_string {
    Ok(value) => assert_eq(value, "hello world")
    Err(_) => assert_true(false)
  }
  
  let bool_json = "true"
  let deserialized_bool = JsonDeserializer::deserialize_bool(deserializer, bool_json)
  match deserialized_bool {
    Ok(value) => assert_eq(value, true)
    Err(_) => assert_true(false)
  }
  
  // Test array deserialization
  let array_json = "[1,2,3,4,5]"
  let deserialized_array = JsonDeserializer::deserialize_int_array(deserializer, array_json)
  match deserialized_array {
    Ok(value) => {
      assert_eq(value.length(), 5)
      assert_eq(value[0], 1)
      assert_eq(value[4], 5)
    }
    Err(_) => assert_true(false)
  }
  
  // Test object deserialization
  let object_json = "{\"name\":\"test\",\"age\":25,\"active\":true}"
  let deserialized_object = JsonDeserializer::deserialize_object(deserializer, object_json)
  match deserialized_object {
    Ok(value) => {
      match JsonObject::get_string(value, "name") {
        Some(name) => assert_eq(name, "test")
        None => assert_true(false)
      }
      
      match JsonObject::get_int(value, "age") {
        Some(age) => assert_eq(age, 25)
        None => assert_true(false)
      }
      
      match JsonObject::get_bool(value, "active") {
        Some(active) => assert_eq(active, true)
        None => assert_true(false)
      }
    }
    Err(_) => assert_true(false)
  }
}

// Test 3: Telemetry Data Serialization
test "telemetry data serialization" {
  let serializer = TelemetrySerializer::new()
  
  // Test Span serialization
  let span_ctx = SpanContext::new("trace123", "span456", true, "test_state")
  let span = Span::new("test_operation", Internal, span_ctx)
  
  Span::set_attribute(span, "service.name", StringValue("test_service"))
  Span::add_event(span, "test_event", Some([("test_attr", StringValue("test_value"))]))
  
  let serialized_span = TelemetrySerializer::serialize_span(serializer, span)
  assert_true(serialized_span.length() > 0)
  
  // Test Metric serialization
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "test_meter")
  let counter = Meter::create_counter(meter, "test_counter", Some("Test counter"), Some("count"))
  
  Counter::add(counter, 42.0)
  let serialized_metric = TelemetrySerializer::serialize_metric(serializer, counter)
  assert_true(serialized_metric.length() > 0)
  
  // Test Log serialization
  let log_record = LogRecord::new(Info, "Test log message")
  let serialized_log = TelemetrySerializer::serialize_log(serializer, log_record)
  assert_true(serialized_log.length() > 0)
  
  // Test Resource serialization
  let resource = Resource::with_attributes(Resource::new(), [
    ("service.name", StringValue("test_service")),
    ("service.version", StringValue("1.0.0"))
  ])
  let serialized_resource = TelemetrySerializer::serialize_resource(serializer, resource)
  assert_true(serialized_resource.length() > 0)
}

// Test 4: Telemetry Data Deserialization
test "telemetry data deserialization" {
  let deserializer = TelemetryDeserializer::new()
  
  // Test Span deserialization
  let span_json = "{\"name\":\"test_operation\",\"kind\":\"Internal\",\"traceId\":\"trace123\",\"spanId\":\"span456\",\"sampled\":true,\"state\":\"test_state\",\"attributes\":{\"service.name\":\"test_service\"},\"events\":[{\"name\":\"test_event\",\"attributes\":{\"test_attr\":\"test_value\"}}]}"
  
  let deserialized_span = TelemetryDeserializer::deserialize_span(deserializer, span_json)
  match deserialized_span {
    Ok(span) => {
      assert_eq(Span::name(span), "test_operation")
      assert_eq(Span::kind(span), Internal)
      
      let ctx = Span::span_context(span)
      assert_eq(SpanContext::trace_id(ctx), "trace123")
      assert_eq(SpanContext::span_id(ctx), "span456")
      assert_true(SpanContext::is_sampled(ctx))
    }
    Err(_) => assert_true(false)
  }
  
  // Test Metric deserialization
  let metric_json = "{\"name\":\"test_counter\",\"type\":\"Counter\",\"description\":\"Test counter\",\"unit\":\"count\",\"value\":42.0}"
  
  let deserialized_metric = TelemetryDeserializer::deserialize_metric(deserializer, metric_json)
  match deserialized_metric {
    Ok(metric) => {
      assert_eq(Metric::name(metric), "test_counter")
      assert_eq(Metric::type(metric), Counter)
      assert_eq(Metric::value(metric), 42.0)
    }
    Err(_) => assert_true(false)
  }
  
  // Test Log deserialization
  let log_json = "{\"severityNumber\":\"Info\",\"body\":\"Test log message\"}"
  
  let deserialized_log = TelemetryDeserializer::deserialize_log(deserializer, log_json)
  match deserialized_log {
    Ok(log) => {
      assert_eq(LogRecord::severity_number(log), Info)
      match LogRecord::body(log) {
        Some(body) => assert_eq(body, "Test log message")
        None => assert_true(false)
      }
    }
    Err(_) => assert_true(false)
  }
  
  // Test Resource deserialization
  let resource_json = "{\"attributes\":{\"service.name\":\"test_service\",\"service.version\":\"1.0.0\"}}"
  
  let deserialized_resource = TelemetryDeserializer::deserialize_resource(deserializer, resource_json)
  match deserialized_resource {
    Ok(resource) => {
      let service_name = Resource::get_attribute(resource, "service.name")
      match service_name {
        Some(StringValue(name)) => assert_eq(name, "test_service")
        _ => assert_true(false)
      }
      
      let service_version = Resource::get_attribute(resource, "service.version")
      match service_version {
        Some(StringValue(version)) => assert_eq(version, "1.0.0")
        _ => assert_true(false)
      }
    }
    Err(_) => assert_true(false)
  }
}

// Test 5: Binary Serialization
test "binary serialization" {
  let serializer = BinarySerializer::new()
  
  // Test primitive type serialization
  let int_value = 42
  let serialized_int = BinarySerializer::serialize_int(serializer, int_value)
  assert_eq(serialized_int.length(), 4) // 4 bytes for int
  
  let float_value = 3.14
  let serialized_float = BinarySerializer::serialize_float(serializer, float_value)
  assert_eq(serialized_float.length(), 8) // 8 bytes for float
  
  let string_value = "hello world"
  let serialized_string = BinarySerializer::serialize_string(serializer, string_value)
  assert_eq(serialized_string.length(), 11 + 4) // 11 chars + 4 bytes for length
  
  // Test array serialization
  let array_value = [1, 2, 3, 4, 5]
  let serialized_array = BinarySerializer::serialize_int_array(serializer, array_value)
  assert_eq(serialized_array.length(), 4 + 5 * 4) // 4 bytes for count + 5 * 4 bytes for ints
  
  // Test complex object serialization
  let object_value = {
    name: "test",
    age: 25,
    active: true,
    scores: [90.5, 85.0, 92.3]
  }
  let serialized_object = BinarySerializer::serialize_object(serializer, object_value)
  assert_true(serialized_object.length() > 0)
}

// Test 6: Binary Deserialization
test "binary deserialization" {
  let deserializer = BinaryDeserializer::new()
  
  // Test primitive type deserialization
  let int_bytes = [0x00, 0x00, 0x00, 0x2A] // 42 in big-endian
  let deserialized_int = BinaryDeserializer::deserialize_int(deserializer, int_bytes)
  match deserialized_int {
    Ok(value) => assert_eq(value, 42)
    Err(_) => assert_true(false)
  }
  
  // Test string deserialization
  let string_bytes = [0x00, 0x00, 0x00, 0x05, 0x68, 0x65, 0x6C, 0x6C, 0x6F] // "hello"
  let deserialized_string = BinaryDeserializer::deserialize_string(deserializer, string_bytes)
  match deserialized_string {
    Ok(value) => assert_eq(value, "hello")
    Err(_) => assert_true(false)
  }
  
  // Test array deserialization
  let array_bytes = [0x00, 0x00, 0x00, 0x03, 0x00, 0x00, 0x00, 0x01, 0x00, 0x00, 0x00, 0x02, 0x00, 0x00, 0x00, 0x03] // [1, 2, 3]
  let deserialized_array = BinaryDeserializer::deserialize_int_array(deserializer, array_bytes)
  match deserialized_array {
    Ok(value) => {
      assert_eq(value.length(), 3)
      assert_eq(value[0], 1)
      assert_eq(value[1], 2)
      assert_eq(value[2], 3)
    }
    Err(_) => assert_true(false)
  }
}

// Test 7: Protocol Buffer Serialization
test "protocol buffer serialization" {
  let serializer = ProtobufSerializer::new()
  
  // Test basic message serialization
  let message = ProtobufMessage::new()
  ProtobufMessage::add_int_field(message, "id", 123)
  ProtobufMessage::add_string_field(message, "name", "test")
  ProtobufMessage::add_bool_field(message, "active", true)
  
  let serialized_message = ProtobufSerializer::serialize(serializer, message)
  assert_true(serialized_message.length() > 0)
  
  // Test nested message serialization
  let nested_message = ProtobufMessage::new()
  ProtobufMessage::add_string_field(nested_message, "address", "123 Main St")
  ProtobufMessage::add_string_field(nested_message, "city", "Anytown")
  
  ProtobufMessage::add_message_field(message, "contact", nested_message)
  
  let serialized_nested = ProtobufSerializer::serialize(serializer, message)
  assert_true(serialized_nested.length() > serialized_message.length())
  
  // Test repeated field serialization
  let scores = [95.5, 87.3, 92.1]
  ProtobufMessage::add_repeated_double_field(message, "scores", scores)
  
  let serialized_repeated = ProtobufSerializer::serialize(serializer, message)
  assert_true(serialized_repeated.length() > serialized_nested.length())
}

// Test 8: Protocol Buffer Deserialization
test "protocol buffer deserialization" {
  let deserializer = ProtobufDeserializer::new()
  
  // Test basic message deserialization
  let message_bytes = [0x08, 0x7B, 0x12, 0x04, 0x74, 0x65, 0x73, 0x74, 0x18, 0x01] // id: 123, name: "test", active: true
  
  let deserialized_message = ProtobufDeserializer::deserialize(deserializer, message_bytes)
  match deserialized_message {
    Ok(message) => {
      match ProtobufMessage::get_int_field(message, "id") {
        Some(value) => assert_eq(value, 123)
        None => assert_true(false)
      }
      
      match ProtobufMessage::get_string_field(message, "name") {
        Some(value) => assert_eq(value, "test")
        None => assert_true(false)
      }
      
      match ProtobufMessage::get_bool_field(message, "active") {
        Some(value) => assert_eq(value, true)
        None => assert_true(false)
      }
    }
    Err(_) => assert_true(false)
  }
  
  // Test nested message deserialization
  let nested_bytes = [0x08, 0x7B, 0x12, 0x04, 0x74, 0x65, 0x73, 0x74, 0x18, 0x01, 0x2A, 0x0F, 0x0A, 0x0B, 0x31, 0x32, 0x33, 0x20, 0x4D, 0x61, 0x69, 0x6E, 0x20, 0x53, 0x74] // with nested address field
  
  let deserialized_nested = ProtobufDeserializer::deserialize(deserializer, nested_bytes)
  match deserialized_nested {
    Ok(message) => {
      match ProtobufMessage::get_message_field(message, "contact") {
        Some(contact_message) => {
          match ProtobufMessage::get_string_field(contact_message, "address") {
            Some(value) => assert_eq(value, "123 Main St")
            None => assert_true(false)
          }
        }
        None => assert_true(false)
      }
    }
    Err(_) => assert_true(false)
  }
}

// Test 9: Custom Serialization Format
test "custom serialization format" {
  let serializer = CustomSerializer::new()
  
  // Define custom serialization rules
  CustomSerializer::add_rule(serializer, "Person", |person| {
    let name = person.name
    let age = person.age
    "Person:" + name + ":" + age.to_string()
  })
  
  CustomSerializer::add_rule(serializer, "Address", |address| {
    let street = address.street
    let city = address.city
    let state = address.state
    "Address:" + street + ":" + city + ":" + state
  })
  
  // Test custom serialization
  let person = {
    name: "John Doe",
    age: 30,
    address: {
      street: "123 Main St",
      city: "Anytown",
      state: "CA"
    }
  }
  
  let serialized_person = CustomSerializer::serialize(serializer, person)
  assert_eq(serialized_person, "Person:John Doe:30")
  
  let serialized_address = CustomSerializer::serialize(serializer, person.address)
  assert_eq(serialized_address, "Address:123 Main St:Anytown:CA")
  
  // Test composite custom serialization
  CustomSerializer::add_composite_rule(serializer, "PersonWithAddress", |person| {
    let person_str = CustomSerializer::serialize(serializer, person)
    let address_str = CustomSerializer::serialize(serializer, person.address)
    person_str + "|" + address_str
  })
  
  let serialized_composite = CustomSerializer::serialize_composite(serializer, person)
  assert_eq(serialized_composite, "Person:John Doe:30|Address:123 Main St:Anytown:CA")
}

// Test 10: Serialization Performance and Compression
test "serialization performance and compression" {
  let serializer = JsonSerializer::new()
  let compressor = GzipCompressor::new()
  
  // Create large telemetry data
  let large_spans = []
  
  for i in 0..=1000 {
    let span_ctx = SpanContext::new("trace" + i.to_string(), "span" + i.to_string(), true, "state" + i.to_string())
    let span = Span::new("operation" + i.to_string(), Internal, span_ctx)
    
    // Add many attributes
    for j in 0..=10 {
      Span::set_attribute(span, "attr" + j.to_string(), StringValue("value" + j.to_string()))
    }
    
    // Add many events
    for j in 0..=5 {
      Span::add_event(span, "event" + j.to_string(), Some([("event_attr", StringValue("event_value" + j.to_string()))]))
    }
    
    large_spans.push(span)
  }
  
  // Serialize without compression
  let start_time = Time::now()
  let serialized_data = []
  
  for span in large_spans {
    let serialized = TelemetrySerializer::serialize_span(serializer, span)
    serialized_data.push(serialized)
  }
  
  let serialize_time = Time::now() - start_time
  let total_size = serialized_data.reduce(|acc, data| acc + data.length(), 0)
  
  // Serialize with compression
  let compressed_data = []
  let start_compress_time = Time::now()
  
  for span in large_spans {
    let serialized = TelemetrySerializer::serialize_span(serializer, span)
    let compressed = Compressor::compress(compressor, serialized)
    compressed_data.push(compressed)
  }
  
  let compress_time = Time::now() - start_compress_time
  let total_compressed_size = compressed_data.reduce(|acc, data| acc + data.length(), 0)
  
  // Verify compression benefits
  assert_true(total_compressed_size < total_size)
  let compression_ratio = (total_compressed_size as Float) / (total_size as Float)
  assert_true(compression_ratio < 0.8) // At least 20% compression
  
  // Test deserialization with decompression
  let decompressor = GzipDecompressor::new()
  let start_decompress_time = Time::now()
  
  for i in 0..=compressed_data.length() - 1 {
    let compressed = compressed_data[i]
    let decompressed = Decompressor::decompress(decompressor, compressed)
    
    let deserialized_span = TelemetryDeserializer::deserialize_span(TelemetryDeserializer::new(), decompressed)
    match deserialized_span {
      Ok(span) => {
        assert_eq(Span::name(span), "operation" + i.to_string())
        
        let ctx = Span::span_context(span)
        assert_eq(SpanContext::trace_id(ctx), "trace" + i.to_string())
        assert_eq(SpanContext::span_id(ctx), "span" + i.to_string())
      }
      Err(_) => assert_true(false)
    }
  }
  
  let decompress_time = Time::now() - start_decompress_time
  
  // Verify performance is reasonable
  assert_true(serialize_time < 10000L) // Less than 10 seconds
  assert_true(compress_time < 15000L) // Less than 15 seconds
  assert_true(decompress_time < 20000L) // Less than 20 seconds
  
  // Test serialization format comparison
  let formats = ["json", "binary", "protobuf", "custom"]
  let format_performance = []
  
  for format in formats {
    let format_serializer = SerializerFactory::create(format)
    let format_start_time = Time::now()
    
    for span in large_spans {
      let serialized = Serializer::serialize(format_serializer, span)
    }
    
    let format_time = Time::now() - format_start_time
    format_performance.push((format, format_time))
  }
  
  // Verify performance differences
  let json_performance = format_performance.find(|p| p.0 == "json")
  let binary_performance = format_performance.find(|p| p.0 == "binary")
  
  match (json_performance, binary_performance) {
    (Some((_, json_time)), Some((_, binary_time))) => {
      // Binary should be faster than JSON
      assert_true(binary_time < json_time)
    }
    _ => assert_true(false)
  }
}