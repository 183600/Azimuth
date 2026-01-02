// Azimuth Telemetry System - Serialization and Deserialization Tests
// This file contains test cases for data serialization, deserialization, and format conversion

// Test 1: JSON Serialization and Deserialization
test "json serialization and deserialization" {
  // Test AttributeValue JSON serialization
  let string_attr = StringValue("test_value")
  let json_string = AttributeValue::to_json(string_attr)
  assert_eq(json_string, "\"test_value\"")
  
  let int_attr = IntValue(42)
  let json_int = AttributeValue::to_json(int_attr)
  assert_eq(json_int, "42")
  
  let bool_attr = BoolValue(true)
  let json_bool = AttributeValue::to_json(bool_attr)
  assert_eq(json_bool, "true")
  
  let float_attr = FloatValue(3.14159)
  let json_float = AttributeValue::to_json(float_attr)
  assert_eq(json_float, "3.14159")
  
  // Test array JSON serialization
  let string_array = ArrayStringValue(["a", "b", "c"])
  let json_array = AttributeValue::to_json(string_array)
  assert_eq(json_array, "[\"a\", \"b\", \"c\"]")
  
  let int_array = ArrayIntValue([1, 2, 3])
  let json_int_array = AttributeValue::to_json(int_array)
  assert_eq(json_int_array, "[1, 2, 3]")
  
  // Test JSON deserialization
  let deserialized_string = AttributeValue::from_json("\"test_value\"")
  match deserialized_string {
    Some(StringValue(value)) => assert_eq(value, "test_value")
    _ => assert_true(false)
  }
  
  let deserialized_int = AttributeValue::from_json("42")
  match deserialized_int {
    Some(IntValue(value)) => assert_eq(value, 42)
    _ => assert_true(false)
  }
  
  let deserialized_bool = AttributeValue::from_json("true")
  match deserialized_bool {
    Some(BoolValue(value)) => assert_eq(value, true)
    _ => assert_true(false)
  }
  
  let deserialized_float = AttributeValue::from_json("3.14159")
  match deserialized_float {
    Some(FloatValue(value)) => assert_eq(value, 3.14159)
    _ => assert_true(false)
  }
  
  // Test array JSON deserialization
  let deserialized_array = AttributeValue::from_json("[\"a\", \"b\", \"c\"]")
  match deserialized_array {
    Some(ArrayStringValue(values)) => {
      assert_eq(values.length(), 3)
      assert_eq(values[0], "a")
      assert_eq(values[1], "b")
      assert_eq(values[2], "c")
    }
    _ => assert_true(false)
  }
  
  let deserialized_int_array = AttributeValue::from_json("[1, 2, 3]")
  match deserialized_int_array {
    Some(ArrayIntValue(values)) => {
      assert_eq(values.length(), 3)
      assert_eq(values[0], 1)
      assert_eq(values[1], 2)
      assert_eq(values[2], 3)
    }
    _ => assert_true(false)
  }
}

// Test 2: Span Serialization and Deserialization
test "span serialization and deserialization" {
  // Create a test span with various properties
  let span_ctx = SpanContext::new("trace_123", "span_456", true, "test_state")
  let span = Span::new("test_span", Server, span_ctx)
  
  // Add events and attributes
  Span::set_status(span, Error, Some("Test error"))
  Span::add_event(span, "event1", Some([("key1", StringValue("value1"))]))
  Span::add_event(span, "event2", Some([("key2", IntValue(42))]))
  
  // Serialize the span
  let serialized = Span::serialize(span)
  assert_true(serialized.length() > 0)
  
  // Deserialize the span
  let deserialized = Span::deserialize(serialized)
  
  match deserialized {
    Some(restored_span) => {
      // Verify basic properties
      assert_eq(Span::name(restored_span), "test_span")
      
      match Span::kind(restored_span) {
        Server => assert_true(true)
        _ => assert_true(false)
      }
      
      let restored_ctx = Span::span_context(restored_span)
      assert_eq(SpanContext::trace_id(restored_ctx), "trace_123")
      assert_eq(SpanContext::span_id(restored_ctx), "span_456")
      assert_true(SpanContext::is_sampled(restored_ctx))
      
      // Verify status
      assert_eq(Span::status(restored_span), Error)
    }
    None => assert_true(false)
  }
  
  // Test serialization of span with links
  let linked_span_ctx = SpanContext::new("trace_789", "span_101112", true, "")
  let linked_span = Span::new("linked_span", Client, linked_span_ctx)
  
  Span::add_link(span, Span::span_context(linked_span))
  let serialized_with_links = Span::serialize(span)
  
  let deserialized_with_links = Span::deserialize(serialized_with_links)
  match deserialized_with_links {
    Some(restored_span) => {
      // Verify links are preserved
      let links = Span::links(restored_span)
      assert_eq(links.length(), 1)
      
      let linked_ctx = links[0]
      assert_eq(SpanContext::trace_id(linked_ctx), "trace_789")
      assert_eq(SpanContext::span_id(linked_ctx), "span_101112")
    }
    None => assert_true(false)
  }
}

// Test 3: Attributes Serialization and Deserialization
test "attributes serialization and deserialization" {
  let attrs = Attributes::new()
  
  // Add various types of attributes
  Attributes::set(attrs, "string.key", StringValue("string_value"))
  Attributes::set(attrs, "int.key", IntValue(123))
  Attributes::set(attrs, "float.key", FloatValue(3.14))
  Attributes::set(attrs, "bool.key", BoolValue(true))
  Attributes::set(attrs, "array.string", ArrayStringValue(["a", "b", "c"]))
  Attributes::set(attrs, "array.int", ArrayIntValue([1, 2, 3]))
  
  // Serialize attributes
  let serialized = Attributes::serialize(attrs)
  assert_true(serialized.length() > 0)
  
  // Deserialize attributes
  let deserialized = Attributes::deserialize(serialized)
  
  match deserialized {
    Some(restored_attrs) => {
      // Verify string attribute
      let string_value = Attributes::get(restored_attrs, "string.key")
      match string_value {
        Some(StringValue(v)) => assert_eq(v, "string_value")
        _ => assert_true(false)
      }
      
      // Verify int attribute
      let int_value = Attributes::get(restored_attrs, "int.key")
      match int_value {
        Some(IntValue(v)) => assert_eq(v, 123)
        _ => assert_true(false)
      }
      
      // Verify float attribute
      let float_value = Attributes::get(restored_attrs, "float.key")
      match float_value {
        Some(FloatValue(v)) => assert_eq(v, 3.14)
        _ => assert_true(false)
      }
      
      // Verify bool attribute
      let bool_value = Attributes::get(restored_attrs, "bool.key")
      match bool_value {
        Some(BoolValue(v)) => assert_true(v)
        _ => assert_true(false)
      }
      
      // Verify string array attribute
      let array_string_value = Attributes::get(restored_attrs, "array.string")
      match array_string_value {
        Some(ArrayStringValue(v)) => {
          assert_eq(v.length(), 3)
          assert_eq(v[0], "a")
          assert_eq(v[1], "b")
          assert_eq(v[2], "c")
        }
        _ => assert_true(false)
      }
      
      // Verify int array attribute
      let array_int_value = Attributes::get(restored_attrs, "array.int")
      match array_int_value {
        Some(ArrayIntValue(v)) => {
          assert_eq(v.length(), 3)
          assert_eq(v[0], 1)
          assert_eq(v[1], 2)
          assert_eq(v[2], 3)
        }
        _ => assert_true(false)
      }
    }
    None => assert_true(false)
  }
}

// Test 4: Binary Serialization and Deserialization
test "binary serialization and deserialization" {
  // Test binary serialization of AttributeValue
  let string_attr = StringValue("binary_test")
  let binary_data = AttributeValue::to_binary(string_attr)
  assert_true(binary_data.length() > 0)
  
  let binary_deserialized = AttributeValue::from_binary(binary_data)
  match binary_deserialized {
    Some(StringValue(v)) => assert_eq(v, "binary_test")
    _ => assert_true(false)
  }
  
  // Test binary serialization of complex attributes
  let attrs = Attributes::new()
  Attributes::set(attrs, "key1", StringValue("value1"))
  Attributes::set(attrs, "key2", IntValue(42))
  Attributes::set(attrs, "key3", BoolValue(true))
  
  let binary_attrs = Attributes::to_binary(attrs)
  let binary_attrs_deserialized = Attributes::from_binary(binary_attrs)
  
  match binary_attrs_deserialized {
    Some(restored_attrs) => {
      let key1_value = Attributes::get(restored_attrs, "key1")
      match key1_value {
        Some(StringValue(v)) => assert_eq(v, "value1")
        _ => assert_true(false)
      }
      
      let key2_value = Attributes::get(restored_attrs, "key2")
      match key2_value {
        Some(IntValue(v)) => assert_eq(v, 42)
        _ => assert_true(false)
      }
      
      let key3_value = Attributes::get(restored_attrs, "key3")
      match key3_value {
        Some(BoolValue(v)) => assert_true(v)
        _ => assert_true(false)
      }
    }
    None => assert_true(false)
  }
  
  // Test binary serialization of Span
  let span_ctx = SpanContext::new("binary_trace", "binary_span", true, "")
  let span = Span::new("binary_test_span", Internal, span_ctx)
  Span::add_event(span, "binary_event", Some([("binary_key", StringValue("binary_value"))]))
  
  let binary_span = Span::to_binary(span)
  let binary_span_deserialized = Span::from_binary(binary_span)
  
  match binary_span_deserialized {
    Some(restored_span) => {
      assert_eq(Span::name(restored_span), "binary_test_span")
      
      match Span::kind(restored_span) {
        Internal => assert_true(true)
        _ => assert_true(false)
      }
      
      let restored_ctx = Span::span_context(restored_span)
      assert_eq(SpanContext::trace_id(restored_ctx), "binary_trace")
      assert_eq(SpanContext::span_id(restored_ctx), "binary_span")
    }
    None => assert_true(false)
  }
}

// Test 5: Protocol Buffer Serialization and Deserialization
test "protocol buffer serialization and deserialization" {
  // Test protobuf serialization of AttributeValue
  let string_attr = StringValue("protobuf_test")
  let protobuf_data = AttributeValue::to_protobuf(string_attr)
  assert_true(protobuf_data.length() > 0)
  
  let protobuf_deserialized = AttributeValue::from_protobuf(protobuf_data)
  match protobuf_deserialized {
    Some(StringValue(v)) => assert_eq(v, "protobuf_test")
    _ => assert_true(false)
  }
  
  // Test protobuf serialization of Span
  let span_ctx = SpanContext::new("protobuf_trace", "protobuf_span", true, "protobuf_state")
  let span = Span::new("protobuf_test_span", Server, span_ctx)
  Span::set_status(span, Ok, Some("Protobuf test completed"))
  
  let protobuf_span = Span::to_protobuf(span)
  let protobuf_span_deserialized = Span::from_protobuf(protobuf_span)
  
  match protobuf_span_deserialized {
    Some(restored_span) => {
      assert_eq(Span::name(restored_span), "protobuf_test_span")
      
      match Span::kind(restored_span) {
        Server => assert_true(true)
        _ => assert_true(false)
      }
      
      let restored_ctx = Span::span_context(restored_span)
      assert_eq(SpanContext::trace_id(restored_ctx), "protobuf_trace")
      assert_eq(SpanContext::span_id(restored_ctx), "protobuf_span")
      assert_eq(SpanContext::state(restored_ctx), "protobuf_state")
      
      assert_eq(Span::status(restored_span), Ok)
    }
    None => assert_true(false)
  }
  
  // Test protobuf serialization of Resource
  let resource = Resource::with_attributes(Resource::new(), [
    ("service.name", StringValue("protobuf_service")),
    ("service.version", StringValue("1.0.0")),
    ("service.instance.id", StringValue("protobuf-instance-123"))
  ])
  
  let protobuf_resource = Resource::to_protobuf(resource)
  let protobuf_resource_deserialized = Resource::from_protobuf(protobuf_resource)
  
  match protobuf_resource_deserialized {
    Some(restored_resource) => {
      let service_name = Resource::get_attribute(restored_resource, "service.name")
      match service_name {
        Some(StringValue(name)) => assert_eq(name, "protobuf_service")
        _ => assert_true(false)
      }
      
      let service_version = Resource::get_attribute(restored_resource, "service.version")
      match service_version {
        Some(StringValue(version)) => assert_eq(version, "1.0.0")
        _ => assert_true(false)
      }
    }
    None => assert_true(false)
  }
}

// Test 6: XML Serialization and Deserialization
test "xml serialization and deserialization" {
  // Test XML serialization of AttributeValue
  let string_attr = StringValue("xml_test")
  let xml_data = AttributeValue::to_xml(string_attr)
  assert_true(xml_data.contains("<string>xml_test</string>"))
  
  let xml_deserialized = AttributeValue::from_xml(xml_data)
  match xml_deserialized {
    Some(StringValue(v)) => assert_eq(v, "xml_test")
    _ => assert_true(false)
  }
  
  // Test XML serialization of Attributes
  let attrs = Attributes::new()
  Attributes::set(attrs, "xml.key1", StringValue("xml_value1"))
  Attributes::set(attrs, "xml.key2", IntValue(42))
  Attributes::set(attrs, "xml.key3", BoolValue(true))
  
  let xml_attrs = Attributes::to_xml(attrs)
  assert_true(xml_attrs.contains("<attributes>"))
  assert_true(xml_attrs.contains("</attributes>"))
  
  let xml_attrs_deserialized = Attributes::from_xml(xml_attrs)
  
  match xml_attrs_deserialized {
    Some(restored_attrs) => {
      let key1_value = Attributes::get(restored_attrs, "xml.key1")
      match key1_value {
        Some(StringValue(v)) => assert_eq(v, "xml_value1")
        _ => assert_true(false)
      }
      
      let key2_value = Attributes::get(restored_attrs, "xml.key2")
      match key2_value {
        Some(IntValue(v)) => assert_eq(v, 42)
        _ => assert_true(false)
      }
      
      let key3_value = Attributes::get(restored_attrs, "xml.key3")
      match key3_value {
        Some(BoolValue(v)) => assert_true(v)
        _ => assert_true(false)
      }
    }
    None => assert_true(false)
  }
}

// Test 7: CSV Serialization and Deserialization
test "csv serialization and deserialization" {
  // Test CSV serialization of array attributes
  let string_array = ArrayStringValue(["value1", "value2", "value3"])
  let csv_data = AttributeValue::to_csv(string_array)
  assert_eq(csv_data, "value1,value2,value3")
  
  let csv_deserialized = AttributeValue::from_csv(csv_data, "string")
  match csv_deserialized {
    Some(ArrayStringValue(values)) => {
      assert_eq(values.length(), 3)
      assert_eq(values[0], "value1")
      assert_eq(values[1], "value2")
      assert_eq(values[2], "value3")
    }
    _ => assert_true(false)
  }
  
  // Test CSV serialization of int array
  let int_array = ArrayIntValue([1, 2, 3, 4, 5])
  let csv_int_data = AttributeValue::to_csv(int_array)
  assert_eq(csv_int_data, "1,2,3,4,5")
  
  let csv_int_deserialized = AttributeValue::from_csv(csv_int_data, "int")
  match csv_int_deserialized {
    Some(ArrayIntValue(values)) => {
      assert_eq(values.length(), 5)
      assert_eq(values[0], 1)
      assert_eq(values[4], 5)
    }
    _ => assert_true(false)
  }
  
  // Test CSV with special characters
  let special_array = ArrayStringValue(["value,with,commas", "value\"with\"quotes", "value\nwith\nnewlines"])
  let csv_special_data = AttributeValue::to_csv(special_array)
  
  let csv_special_deserialized = AttributeValue::from_csv(csv_special_data, "string")
  match csv_special_deserialized {
    Some(ArrayStringValue(values)) => {
      assert_eq(values.length(), 3)
      assert_eq(values[0], "value,with,commas")
      assert_eq(values[1], "value\"with\"quotes")
      assert_eq(values[2], "value\nwith\nnewlines")
    }
    _ => assert_true(false)
  }
}

// Test 8: MessagePack Serialization and Deserialization
test "messagepack serialization and deserialization" {
  // Test MessagePack serialization of AttributeValue
  let string_attr = StringValue("messagepack_test")
  let msgpack_data = AttributeValue::to_msgpack(string_attr)
  assert_true(msgpack_data.length() > 0)
  
  let msgpack_deserialized = AttributeValue::from_msgpack(msgpack_data)
  match msgpack_deserialized {
    Some(StringValue(v)) => assert_eq(v, "messagepack_test")
    _ => assert_true(false)
  }
  
  // Test MessagePack serialization of complex data
  let attrs = Attributes::new()
  Attributes::set(attrs, "msgpack.key1", StringValue("msgpack_value1"))
  Attributes::set(attrs, "msgpack.key2", IntValue(42))
  Attributes::set(attrs, "msgpack.key3", FloatValue(3.14))
  Attributes::set(attrs, "msgpack.key4", BoolValue(true))
  Attributes::set(attrs, "msgpack.key5", ArrayStringValue(["a", "b", "c"]))
  
  let msgpack_attrs = Attributes::to_msgpack(attrs)
  let msgpack_attrs_deserialized = Attributes::from_msgpack(msgpack_attrs)
  
  match msgpack_attrs_deserialized {
    Some(restored_attrs) => {
      let key1_value = Attributes::get(restored_attrs, "msgpack.key1")
      match key1_value {
        Some(StringValue(v)) => assert_eq(v, "msgpack_value1")
        _ => assert_true(false)
      }
      
      let key2_value = Attributes::get(restored_attrs, "msgpack.key2")
      match key2_value {
        Some(IntValue(v)) => assert_eq(v, 42)
        _ => assert_true(false)
      }
      
      let key3_value = Attributes::get(restored_attrs, "msgpack.key3")
      match key3_value {
        Some(FloatValue(v)) => assert_eq(v, 3.14)
        _ => assert_true(false)
      }
      
      let key4_value = Attributes::get(restored_attrs, "msgpack.key4")
      match key4_value {
        Some(BoolValue(v)) => assert_true(v)
        _ => assert_true(false)
      }
      
      let key5_value = Attributes::get(restored_attrs, "msgpack.key5")
      match key5_value {
        Some(ArrayStringValue(v)) => {
          assert_eq(v.length(), 3)
          assert_eq(v[0], "a")
          assert_eq(v[1], "b")
          assert_eq(v[2], "c")
        }
        _ => assert_true(false)
      }
    }
    None => assert_true(false)
  }
}