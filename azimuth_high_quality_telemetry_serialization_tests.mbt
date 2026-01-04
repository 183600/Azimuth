// Azimuth Telemetry System - High Quality Telemetry Serialization Tests
// This file contains comprehensive test cases for telemetry data serialization and deserialization

// Test 1: Basic Attribute Value Serialization
test "attribute value serialization and deserialization" {
  // Test string attribute serialization
  let string_attr = StringValue("test_value")
  let serialized_string = AttributeValue::serialize(string_attr)
  let deserialized_string = AttributeValue::deserialize(serialized_string)
  
  match deserialized_string {
    Some(StringValue(v)) => assert_eq(v, "test_value")
    _ => assert_true(false)
  }
  
  // Test int attribute serialization
  let int_attr = IntValue(42)
  let serialized_int = AttributeValue::serialize(int_attr)
  let deserialized_int = AttributeValue::deserialize(serialized_int)
  
  match deserialized_int {
    Some(IntValue(v)) => assert_eq(v, 42)
    _ => assert_true(false)
  }
  
  // Test float attribute serialization
  let float_attr = FloatValue(3.14159)
  let serialized_float = AttributeValue::serialize(float_attr)
  let deserialized_float = AttributeValue::deserialize(serialized_float)
  
  match deserialized_float {
    Some(FloatValue(v)) => assert_true(abs(v - 3.14159) < 0.00001)
    _ => assert_true(false)
  }
  
  // Test bool attribute serialization
  let bool_attr = BoolValue(true)
  let serialized_bool = AttributeValue::serialize(bool_attr)
  let deserialized_bool = AttributeValue::deserialize(serialized_bool)
  
  match deserialized_bool {
    Some(BoolValue(v)) => assert_true(v)
    _ => assert_true(false)
  }
}

// Test 2: Complex Attribute Array Serialization
test "complex attribute array serialization" {
  // Test array string attribute serialization
  let array_string_attr = ArrayStringValue(["item1", "item2", "item3"])
  let serialized_array_string = AttributeValue::serialize(array_string_attr)
  let deserialized_array_string = AttributeValue::deserialize(serialized_array_string)
  
  match deserialized_array_string {
    Some(ArrayStringValue(v)) => {
      assert_eq(v.length(), 3)
      assert_eq(v[0], "item1")
      assert_eq(v[1], "item2")
      assert_eq(v[2], "item3")
    }
    _ => assert_true(false)
  }
  
  // Test array int attribute serialization
  let array_int_attr = ArrayIntValue([10, 20, 30, 40, 50])
  let serialized_array_int = AttributeValue::serialize(array_int_attr)
  let deserialized_array_int = AttributeValue::deserialize(serialized_array_int)
  
  match deserialized_array_int {
    Some(ArrayIntValue(v)) => {
      assert_eq(v.length(), 5)
      assert_eq(v[0], 10)
      assert_eq(v[4], 50)
    }
    _ => assert_true(false)
  }
  
  // Test nested array serialization
  let nested_array = ArrayStringValue(["nested1,nested2", "nested3,nested4"])
  let serialized_nested = AttributeValue::serialize(nested_array)
  let deserialized_nested = AttributeValue::deserialize(serialized_nested)
  
  match deserialized_nested {
    Some(ArrayStringValue(v)) => {
      assert_eq(v.length(), 2)
      assert_eq(v[0], "nested1,nested2")
      assert_eq(v[1], "nested3,nested4")
    }
    _ => assert_true(false)
  }
}

// Test 3: Span Context Serialization
test "span context serialization" {
  let trace_id = "0af7651916cd43dd8448eb211c80319c"
  let span_id = "b7ad6b7169203331"
  let span_ctx = SpanContext::new(trace_id, span_id, true, "test_state")
  
  // Serialize span context
  let serialized_ctx = SpanContext::serialize(span_ctx)
  assert_true(serialized_ctx.length() > 0)
  
  // Deserialize span context
  let deserialized_ctx = SpanContext::deserialize(serialized_ctx)
  
  match deserialized_ctx {
    Some(ctx) => {
      assert_eq(SpanContext::trace_id(ctx), trace_id)
      assert_eq(SpanContext::span_id(ctx), span_id)
      assert_true(SpanContext::is_sampled(ctx))
      assert_true(SpanContext::is_valid(ctx))
    }
    None => assert_true(false)
  }
  
  // Test invalid span context serialization
  let invalid_ctx = SpanContext::new("", "", false, "")
  let serialized_invalid = SpanContext::serialize(invalid_ctx)
  let deserialized_invalid = SpanContext::deserialize(serialized_invalid)
  
  match deserialized_invalid {
    Some(ctx) => {
      assert_false(SpanContext::is_valid(ctx))
      assert_false(SpanContext::is_sampled(ctx))
    }
    None => assert_true(false)
  }
}

// Test 4: Attributes Map Serialization
test "attributes map serialization" {
  let attrs = Attributes::new()
  
  // Set various attribute types
  Attributes::set(attrs, "string.key", StringValue("test_value"))
  Attributes::set(attrs, "int.key", IntValue(123))
  Attributes::set(attrs, "float.key", FloatValue(45.67))
  Attributes::set(attrs, "bool.key", BoolValue(true))
  Attributes::set(attrs, "array.string.key", ArrayStringValue(["a", "b", "c"]))
  Attributes::set(attrs, "array.int.key", ArrayIntValue([1, 2, 3]))
  
  // Serialize attributes
  let serialized_attrs = Attributes::serialize(attrs)
  assert_true(serialized_attrs.length() > 0)
  
  // Deserialize attributes
  let deserialized_attrs = Attributes::deserialize(serialized_attrs)
  
  match deserialized_attrs {
    Some(deserialized) => {
      // Verify string attribute
      let string_result = Attributes::get(deserialized, "string.key")
      match string_result {
        Some(StringValue(v)) => assert_eq(v, "test_value")
        _ => assert_true(false)
      }
      
      // Verify int attribute
      let int_result = Attributes::get(deserialized, "int.key")
      match int_result {
        Some(IntValue(v)) => assert_eq(v, 123)
        _ => assert_true(false)
      }
      
      // Verify float attribute
      let float_result = Attributes::get(deserialized, "float.key")
      match float_result {
        Some(FloatValue(v)) => assert_true(abs(v - 45.67) < 0.00001)
        _ => assert_true(false)
      }
      
      // Verify bool attribute
      let bool_result = Attributes::get(deserialized, "bool.key")
      match bool_result {
        Some(BoolValue(v)) => assert_true(v)
        _ => assert_true(false)
      }
      
      // Verify array string attribute
      let array_string_result = Attributes::get(deserialized, "array.string.key")
      match array_string_result {
        Some(ArrayStringValue(v)) => {
          assert_eq(v.length(), 3)
          assert_eq(v[0], "a")
          assert_eq(v[1], "b")
          assert_eq(v[2], "c")
        }
        _ => assert_true(false)
      }
      
      // Verify array int attribute
      let array_int_result = Attributes::get(deserialized, "array.int.key")
      match array_int_result {
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

// Test 5: Span Serialization with Events
test "span serialization with events" {
  let span_ctx = SpanContext::new("trace_id", "span_id", true, "test_state")
  let span = Span::new("test_span", Internal, span_ctx)
  
  // Add events to span
  Span::add_event(span, "event1", Some([("attr1", StringValue("value1"))]))
  Span::add_event(span, "event2", Some([("attr2", IntValue(42))]))
  
  // Set span status
  Span::set_status(span, Ok, Some("Operation completed successfully"))
  
  // Serialize span
  let serialized_span = Span::serialize(span)
  assert_true(serialized_span.length() > 0)
  
  // Deserialize span
  let deserialized_span = Span::deserialize(serialized_span)
  
  match deserialized_span {
    Some(ds) => {
      // Verify basic span properties
      assert_eq(Span::name(ds), "test_span")
      match Span::kind(ds) {
        Internal => assert_true(true)
        _ => assert_true(false)
      }
      
      // Verify span context
      let ds_ctx = Span::span_context(ds)
      assert_eq(SpanContext::trace_id(ds_ctx), "trace_id")
      assert_eq(SpanContext::span_id(ds_ctx), "span_id")
      
      // Verify status
      assert_eq(Span::status(ds), Ok)
      
      // Note: Event verification would depend on the specific implementation
      // This is a simplified test that focuses on serialization/deserialization
    }
    None => assert_true(false)
  }
}

// Test 6: Log Record Serialization
test "log record serialization" {
  let attrs = Attributes::new()
  Attributes::set(attrs, "log.level", StringValue("info"))
  Attributes::set(attrs, "service.name", StringValue("test_service"))
  
  let log_record = LogRecord::new_with_context(
    Info,
    Some("Test log message with attributes"),
    Some(attrs),
    Some(1234567890L),
    Some(1234567891L),
    Some("trace_id"),
    Some("span_id"),
    None
  )
  
  // Serialize log record
  let serialized_log = LogRecord::serialize(log_record)
  assert_true(serialized_log.length() > 0)
  
  // Deserialize log record
  let deserialized_log = LogRecord::deserialize(serialized_log)
  
  match deserialized_log {
    Some(dl) => {
      // Verify log record properties
      assert_eq(LogRecord::severity_number(dl), Info)
      match LogRecord::body(dl) {
        Some(body) => assert_eq(body, "Test log message with attributes")
        None => assert_true(false)
      }
      assert_eq(LogRecord::trace_id(dl), Some("trace_id"))
      assert_eq(LogRecord::span_id(dl), Some("span_id"))
      
      // Verify attributes
      let log_attrs = LogRecord::attributes(dl)
      match log_attrs {
        Some(attrs) => {
          let level_attr = Attributes::get(attrs, "log.level")
          match level_attr {
            Some(StringValue(v)) => assert_eq(v, "info")
            _ => assert_true(false)
          }
          
          let service_attr = Attributes::get(attrs, "service.name")
          match service_attr {
            Some(StringValue(v)) => assert_eq(v, "test_service")
            _ => assert_true(false)
          }
        }
        None => assert_true(false)
      }
    }
    None => assert_true(false)
  }
}

// Test 7: Resource Serialization
test "resource serialization" {
  let attrs = [
    ("service.name", StringValue("test_service")),
    ("service.version", StringValue("1.0.0")),
    ("service.instance.id", StringValue("instance-123")),
    ("host.name", StringValue("test-host")),
    ("os.type", StringValue("linux"))
  ]
  let resource = Resource::with_attributes(Resource::new(), attrs)
  
  // Serialize resource
  let serialized_resource = Resource::serialize(resource)
  assert_true(serialized_resource.length() > 0)
  
  // Deserialize resource
  let deserialized_resource = Resource::deserialize(serialized_resource)
  
  match deserialized_resource {
    Some(dr) => {
      // Verify resource attributes
      let service_name = Resource::get_attribute(dr, "service.name")
      match service_name {
        Some(StringValue(name)) => assert_eq(name, "test_service")
        _ => assert_true(false)
      }
      
      let service_version = Resource::get_attribute(dr, "service.version")
      match service_version {
        Some(StringValue(version)) => assert_eq(version, "1.0.0")
        _ => assert_true(false)
      }
      
      let instance_id = Resource::get_attribute(dr, "service.instance.id")
      match instance_id {
        Some(StringValue(id)) => assert_eq(id, "instance-123")
        _ => assert_true(false)
      }
      
      let host_name = Resource::get_attribute(dr, "host.name")
      match host_name {
        Some(StringValue(host)) => assert_eq(host, "test-host")
        _ => assert_true(false)
      }
      
      let os_type = Resource::get_attribute(dr, "os.type")
      match os_type {
        Some(StringValue(os)) => assert_eq(os, "linux")
        _ => assert_true(false)
      }
    }
    None => assert_true(false)
  }
}

// Test 8: Metrics Serialization
test "metrics serialization" {
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "test_meter")
  
  // Create counter with attributes
  let counter = Meter::create_counter(meter, "test_counter", Some("Test counter"), Some("count"))
  Counter::add(counter, 1.0)
  Counter::add(counter, 2.5, Some(Attributes::new()))
  
  // Create histogram with attributes
  let histogram = Meter::create_histogram(meter, "test_histogram", Some("Test histogram"), Some("ms"))
  Histogram::record(histogram, 100.0)
  Histogram::record(histogram, 200.5, Some(Attributes::new()))
  
  // Serialize metrics
  let serialized_metrics = Meter::serialize_metrics(meter)
  assert_true(serialized_metrics.length() > 0)
  
  // Deserialize metrics
  let deserialized_metrics = Meter::deserialize_metrics(serialized_metrics)
  
  match deserialized_metrics {
    Some(dm) => {
      // Verify metrics count
      assert_true(Meter::metrics_count(dm) >= 2)
      
      // Note: Specific metric verification would depend on the implementation
      // This test focuses on serialization/deserialization functionality
    }
    None => assert_true(false)
  }
}

// Test 9: Batch Serialization Performance
test "batch serialization performance" {
  let batch_size = 100
  let mut spans = []
  
  // Create multiple spans
  for i in 0..batch_size {
    let span_ctx = SpanContext::new("trace_id_" + i.to_string(), "span_id_" + i.to_string(), true, "")
    let span = Span::new("span_" + i.to_string(), Internal, span_ctx)
    Span::add_event(span, "event_" + i.to_string(), None)
    spans = spans.push(span)
  }
  
  // Serialize batch of spans
  let start_time = Time::now()
  let serialized_batch = Span::serialize_batch(spans)
  let serialization_time = Time::now() - start_time
  
  // Verify serialization completed within reasonable time
  assert_true(serialization_time < 1000) // Less than 1 second
  assert_true(serialized_batch.length() > 0)
  
  // Deserialize batch of spans
  let start_time = Time::now()
  let deserialized_batch = Span::deserialize_batch(serialized_batch)
  let deserialization_time = Time::now() - start_time
  
  // Verify deserialization completed within reasonable time
  assert_true(deserialization_time < 1000) // Less than 1 second
  
  match deserialized_batch {
    Some(spans) => {
      assert_eq(spans.length(), batch_size)
      
      // Verify first and last spans
      let first_span = spans[0]
      assert_eq(Span::name(first_span), "span_0")
      
      let last_span = spans[batch_size - 1]
      assert_eq(Span::name(last_span), "span_" + (batch_size - 1).to_string())
    }
    None => assert_true(false)
  }
}

// Test 10: Error Handling in Serialization
test "error handling in serialization" {
  // Test serialization of invalid data
  let invalid_span_ctx = SpanContext::new("", "", false, "")
  let invalid_span = Span::new("", Internal, invalid_span_ctx)
  
  // Should handle invalid data gracefully
  let serialized_invalid = Span::serialize(invalid_span)
  assert_true(serialized_invalid.length() > 0)
  
  let deserialized_invalid = Span::deserialize(serialized_invalid)
  match deserialized_invalid {
    Some(span) => {
      assert_false(SpanContext::is_valid(Span::span_context(span)))
    }
    None => assert_true(false)
  }
  
  // Test deserialization of corrupted data
  let corrupted_data = "invalid_serialized_data"
  let deserialized_corrupted = Span::deserialize(corrupted_data)
  
  match deserialized_corrupted {
    Some(_) => assert_true(false) // Should not succeed with corrupted data
    None => assert_true(true) // Expected behavior
  }
  
  // Test deserialization of empty data
  let empty_data = ""
  let deserialized_empty = Span::deserialize(empty_data)
  
  match deserialized_empty {
    Some(_) => assert_true(false) // Should not succeed with empty data
    None => assert_true(true) // Expected behavior
  }
}