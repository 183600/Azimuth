// Azimuth Telemetry System - Data Serialization and Deserialization Tests
// This file contains test cases for data serialization and deserialization functionality

// Test 1: JSON Serialization and Deserialization
test "json serialization and deserialization" {
  // Test string serialization
  let string_attr = StringValue("test_value")
  let serialized_string = Serialization::to_json(string_attr)
  let deserialized_string = Serialization::from_json(serialized_string)
  
  match deserialized_string {
    StringValue(v) => assert_eq(v, "test_value")
    _ => assert_true(false)
  }
  
  // Test int serialization
  let int_attr = IntValue(42)
  let serialized_int = Serialization::to_json(int_attr)
  let deserialized_int = Serialization::from_json(serialized_int)
  
  match deserialized_int {
    IntValue(v) => assert_eq(v, 42)
    _ => assert_true(false)
  }
  
  // Test float serialization
  let float_attr = FloatValue(3.14)
  let serialized_float = Serialization::to_json(float_attr)
  let deserialized_float = Serialization::from_json(serialized_float)
  
  match deserialized_float {
    FloatValue(v) => assert_eq(v, 3.14)
    _ => assert_true(false)
  }
  
  // Test bool serialization
  let bool_attr = BoolValue(true)
  let serialized_bool = Serialization::to_json(bool_attr)
  let deserialized_bool = Serialization::from_json(serialized_bool)
  
  match deserialized_bool {
    BoolValue(v) => assert_true(v)
    _ => assert_true(false)
  }
  
  // Test array serialization
  let array_attr = ArrayStringValue(["a", "b", "c"])
  let serialized_array = Serialization::to_json(array_attr)
  let deserialized_array = Serialization::from_json(serialized_array)
  
  match deserialized_array {
    ArrayStringValue(v) => {
      assert_eq(v.length(), 3)
      assert_eq(v[0], "a")
      assert_eq(v[1], "b")
      assert_eq(v[2], "c")
    }
    _ => assert_true(false)
  }
}

// Test 2: Attributes Serialization
test "attributes serialization" {
  let attrs = Attributes::new()
  
  // Set various attribute types
  Attributes::set(attrs, "string.key", StringValue("test_value"))
  Attributes::set(attrs, "int.key", IntValue(42))
  Attributes::set(attrs, "float.key", FloatValue(3.14))
  Attributes::set(attrs, "bool.key", BoolValue(true))
  Attributes::set(attrs, "array.key", ArrayStringValue(["a", "b", "c"]))
  
  // Serialize attributes
  let serialized_attrs = Attributes::to_json(attrs)
  assert_true(serialized_attrs.length() > 0)
  
  // Deserialize attributes
  let deserialized_attrs = Attributes::from_json(serialized_attrs)
  
  // Verify deserialized attributes
  let string_result = Attributes::get(deserialized_attrs, "string.key")
  match string_result {
    Some(StringValue(v)) => assert_eq(v, "test_value")
    _ => assert_true(false)
  }
  
  let int_result = Attributes::get(deserialized_attrs, "int.key")
  match int_result {
    Some(IntValue(v)) => assert_eq(v, 42)
    _ => assert_true(false)
  }
  
  let float_result = Attributes::get(deserialized_attrs, "float.key")
  match float_result {
    Some(FloatValue(v)) => assert_eq(v, 3.14)
    _ => assert_true(false)
  }
  
  let bool_result = Attributes::get(deserialized_attrs, "bool.key")
  match bool_result {
    Some(BoolValue(v)) => assert_true(v)
    _ => assert_true(false)
  }
  
  let array_result = Attributes::get(deserialized_attrs, "array.key")
  match array_result {
    Some(ArrayStringValue(v)) => {
      assert_eq(v.length(), 3)
      assert_eq(v[0], "a")
      assert_eq(v[1], "b")
      assert_eq(v[2], "c")
    }
    _ => assert_true(false)
  }
}

// Test 3: SpanContext Serialization
test "span context serialization" {
  let trace_id = "0af7651916cd43dd8448eb211c80319c"
  let span_id = "b7ad6b7169203331"
  let span_ctx = SpanContext::new(trace_id, span_id, true, "test_state")
  
  // Serialize span context
  let serialized_ctx = SpanContext::to_json(span_ctx)
  assert_true(serialized_ctx.length() > 0)
  
  // Deserialize span context
  let deserialized_ctx = SpanContext::from_json(serialized_ctx)
  
  // Verify deserialized span context
  assert_eq(SpanContext::trace_id(deserialized_ctx), trace_id)
  assert_eq(SpanContext::span_id(deserialized_ctx), span_id)
  assert_true(SpanContext::is_valid(deserialized_ctx))
  assert_true(SpanContext::is_sampled(deserialized_ctx))
}

// Test 4: LogRecord Serialization
test "log record serialization" {
  let attrs = Attributes::new()
  Attributes::set(attrs, "log.level", StringValue("info"))
  Attributes::set(attrs, "log.source", StringValue("test_module"))
  
  let log_record = LogRecord::new_with_context(
    Info,
    Some("Test log message"),
    Some(attrs),
    Some(1234567890L),
    Some(1234567891L),
    Some("trace_id"),
    Some("span_id"),
    Some(Context::root())
  )
  
  // Serialize log record
  let serialized_log = LogRecord::to_json(log_record)
  assert_true(serialized_log.length() > 0)
  
  // Deserialize log record
  let deserialized_log = LogRecord::from_json(serialized_log)
  
  // Verify deserialized log record
  assert_eq(LogRecord::severity_number(deserialized_log), Info)
  match LogRecord::body(deserialized_log) {
    Some(body) => assert_eq(body, "Test log message")
    None => assert_true(false)
  }
  assert_eq(LogRecord::trace_id(deserialized_log), Some("trace_id"))
  assert_eq(LogRecord::span_id(deserialized_log), Some("span_id"))
}

// Test 5: Resource Serialization
test "resource serialization" {
  let resource = Resource::new()
  
  // Add attributes to resource
  let attrs = [
    ("service.name", StringValue("test_service")),
    ("service.version", StringValue("1.0.0")),
    ("service.instance.id", StringValue("instance-123")),
    ("host.name", StringValue("test-host")),
    ("os.type", StringValue("linux"))
  ]
  let resource_with_attrs = Resource::with_attributes(resource, attrs)
  
  // Serialize resource
  let serialized_resource = Resource::to_json(resource_with_attrs)
  assert_true(serialized_resource.length() > 0)
  
  // Deserialize resource
  let deserialized_resource = Resource::from_json(serialized_resource)
  
  // Verify deserialized resource attributes
  let service_name = Resource::get_attribute(deserialized_resource, "service.name")
  match service_name {
    Some(StringValue(name)) => assert_eq(name, "test_service")
    _ => assert_true(false)
  }
  
  let service_version = Resource::get_attribute(deserialized_resource, "service.version")
  match service_version {
    Some(StringValue(version)) => assert_eq(version, "1.0.0")
    _ => assert_true(false)
  }
  
  let service_instance_id = Resource::get_attribute(deserialized_resource, "service.instance.id")
  match service_instance_id {
    Some(StringValue(instance_id)) => assert_eq(instance_id, "instance-123")
    _ => assert_true(false)
  }
}

// Test 6: Metrics Serialization
test "metrics serialization" {
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "test_meter")
  
  // Create a counter and add measurements
  let counter = Meter::create_counter(meter, "test_counter", Some("Test counter"), Some("count"))
  Counter::add(counter, 1.0, Some(Attributes::new()))
  Counter::add(counter, 2.5, Some(Attributes::new()))
  
  // Serialize metrics
  let serialized_metrics = Meter::to_json(meter)
  assert_true(serialized_metrics.length() > 0)
  
  // Deserialize metrics
  let deserialized_meter = Meter::from_json(serialized_metrics)
  
  // Verify deserialized metrics
  let instruments = Meter::get_instruments(deserialized_meter)
  assert_true(instruments.length() > 0)
  
  // Find the counter instrument
  let mut found_counter = false
  for instrument in instruments {
    if Instrument::name(instrument) == "test_counter" {
      found_counter = true
      assert_eq(Instrument::description(instrument), Some("Test counter"))
      assert_eq(Instrument::unit(instrument), Some("count"))
    }
  }
  assert_true(found_counter)
}

// Test 7: Error Handling in Serialization
test "error handling in serialization" {
  // Test serialization of invalid data
  let invalid_string = StringValue("")
  let serialized_invalid = Serialization::to_json(invalid_string)
  
  // Should still serialize without errors
  assert_true(serialized_invalid.length() > 0)
  
  // Test deserialization of invalid JSON
  let invalid_json = "{ invalid json }"
  let deserialized_invalid = Serialization::from_json(invalid_json)
  
  // Should handle gracefully and return a default value
  match deserialized_invalid {
    StringValue(v) => assert_eq(v, "")
    _ => assert_true(false)
  }
  
  // Test deserialization of empty JSON
  let empty_json = ""
  let deserialized_empty = Serialization::from_json(empty_json)
  
  // Should handle gracefully and return a default value
  match deserialized_empty {
    StringValue(v) => assert_eq(v, "")
    _ => assert_true(false)
  }
}

// Test 8: Binary Serialization
test "binary serialization" {
  // Test binary serialization of simple data
  let int_attr = IntValue(42)
  let binary_data = Serialization::to_binary(int_attr)
  assert_true(binary_data.length() > 0)
  
  // Test binary deserialization
  let deserialized_int = Serialization::from_binary(binary_data)
  match deserialized_int {
    IntValue(v) => assert_eq(v, 42)
    _ => assert_true(false)
  }
  
  // Test binary serialization of complex data
  let attrs = Attributes::new()
  Attributes::set(attrs, "key1", StringValue("value1"))
  Attributes::set(attrs, "key2", IntValue(123))
  let binary_attrs = Attributes::to_binary(attrs)
  assert_true(binary_attrs.length() > 0)
  
  // Test binary deserialization of complex data
  let deserialized_attrs = Attributes::from_binary(binary_attrs)
  let result1 = Attributes::get(deserialized_attrs, "key1")
  match result1 {
    Some(StringValue(v)) => assert_eq(v, "value1")
    _ => assert_true(false)
  }
  
  let result2 = Attributes::get(deserialized_attrs, "key2")
  match result2 {
    Some(IntValue(v)) => assert_eq(v, 123)
    _ => assert_true(false)
  }
}

// Test 9: Cross-format Serialization
test "cross-format serialization" {
  // Test JSON to binary conversion
  let string_attr = StringValue("test_value")
  let json_data = Serialization::to_json(string_attr)
  let binary_data = Serialization::json_to_binary(json_data)
  
  // Test binary to JSON conversion
  let json_from_binary = Serialization::binary_to_json(binary_data)
  let deserialized_attr = Serialization::from_json(json_from_binary)
  
  match deserialized_attr {
    StringValue(v) => assert_eq(v, "test_value")
    _ => assert_true(false)
  }
}

// Test 10: Serialization Performance
test "serialization performance" {
  // Create a large set of attributes
  let attrs = Attributes::new()
  for i in 0..=100 {
    let key = "key_" + i.to_string()
    let value = "value_" + i.to_string()
    Attributes::set(attrs, key, StringValue(value))
  }
  
  // Measure serialization time
  let start_time = Time::now()
  let serialized_attrs = Attributes::to_json(attrs)
  let serialization_time = Time::now() - start_time
  
  // Serialization should complete within reasonable time
  assert_true(serialization_time < 1000) // Less than 1000ms
  
  // Measure deserialization time
  start_time = Time::now()
  let deserialized_attrs = Attributes::from_json(serialized_attrs)
  let deserialization_time = Time::now() - start_time
  
  // Deserialization should complete within reasonable time
  assert_true(deserialization_time < 1000) // Less than 1000ms
  
  // Verify data integrity
  for i in 0..=100 {
    let key = "key_" + i.to_string()
    let expected_value = "value_" + i.to_string()
    let result = Attributes::get(deserialized_attrs, key)
    match result {
      Some(StringValue(v)) => assert_eq(v, expected_value)
      _ => assert_true(false)
    }
  }
}