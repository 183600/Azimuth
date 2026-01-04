// Azimuth Telemetry System - Serialization and Deserialization Tests
// This file contains comprehensive serialization and deserialization test cases for the telemetry system

// Test 1: Span Serialization and Deserialization
test "span serialization and deserialization" {
  let provider = TracerProvider::default()
  let tracer = TracerProvider::get_tracer(provider, "serialization_tracer")
  let span = Tracer::start_span(tracer, "serialization_span")
  
  // Add attributes
  Span::set_attribute(span, "string_attr", StringValue("string_value"))
  Span::set_attribute(span, "int_attr", IntValue(42))
  Span::set_attribute(span, "float_attr", FloatValue(3.14))
  Span::set_attribute(span, "bool_attr", BoolValue(true))
  Span::set_attribute(span, "array_string_attr", ArrayStringValue(["value1", "value2", "value3"]))
  Span::set_attribute(span, "array_int_attr", ArrayIntValue([1, 2, 3]))
  
  // Add events
  let event_attrs = [("event_attr", StringValue("event_value"))]
  Span::add_event(span, "test_event", Some(event_attrs))
  
  // Set status
  Span::set_status(span, Ok, Some("Operation completed"))
  
  // End span
  Span::end(span)
  
  // Serialize span
  let serialized = Span::serialize(span)
  assert_true(serialized.length() > 0, "Serialized span should not be empty")
  
  // Deserialize span
  let deserialized = Span::deserialize(serialized)
  assert_true(deserialized != None, "Deserialized span should not be None")
  
  match deserialized {
    Some(deserialized_span) => {
      // Verify span name
      assert_eq(Span::name(deserialized_span), Span::name(span))
      
      // Verify span kind
      assert_eq(Span::kind(deserialized_span), Span::kind(span))
      
      // Verify span context
      let original_ctx = Span::span_context(span)
      let deserialized_ctx = Span::span_context(deserialized_span)
      assert_eq(SpanContext::trace_id(original_ctx), SpanContext::trace_id(deserialized_ctx))
      assert_eq(SpanContext::span_id(original_ctx), SpanContext::span_id(deserialized_ctx))
      assert_eq(SpanContext::is_sampled(original_ctx), SpanContext::is_sampled(deserialized_ctx))
      
      // Verify attributes
      let string_attr = Span::get_attribute(deserialized_span, "string_attr")
      match string_attr {
        Some(StringValue(value)) => assert_eq(value, "string_value"),
        _ => assert_true(false, "String attribute should be preserved")
      }
      
      let int_attr = Span::get_attribute(deserialized_span, "int_attr")
      match int_attr {
        Some(IntValue(value)) => assert_eq(value, 42),
        _ => assert_true(false, "Int attribute should be preserved")
      }
      
      let float_attr = Span::get_attribute(deserialized_span, "float_attr")
      match float_attr {
        Some(FloatValue(value)) => assert_true(abs(value - 3.14) < 0.0001),
        _ => assert_true(false, "Float attribute should be preserved")
      }
      
      let bool_attr = Span::get_attribute(deserialized_span, "bool_attr")
      match bool_attr {
        Some(BoolValue(value)) => assert_true(value),
        _ => assert_true(false, "Bool attribute should be preserved")
      }
      
      let array_string_attr = Span::get_attribute(deserialized_span, "array_string_attr")
      match array_string_attr {
        Some(ArrayStringValue(value)) => {
          assert_eq(value.length(), 3)
          assert_eq(value[0], "value1")
          assert_eq(value[1], "value2")
          assert_eq(value[2], "value3")
        }
        _ => assert_true(false, "Array string attribute should be preserved")
      }
      
      let array_int_attr = Span::get_attribute(deserialized_span, "array_int_attr")
      match array_int_attr {
        Some(ArrayIntValue(value)) => {
          assert_eq(value.length(), 3)
          assert_eq(value[0], 1)
          assert_eq(value[1], 2)
          assert_eq(value[2], 3)
        }
        _ => assert_true(false, "Array int attribute should be preserved")
      }
      
      // Verify events
      let events = Span::get_events(deserialized_span)
      assert_eq(events.length(), 1)
      match events[0] {
        Event(name, attrs) => {
          assert_eq(name, "test_event")
          assert_eq(attrs.length(), 1)
          match attrs[0] {
            (key, StringValue(value)) => {
              assert_eq(key, "event_attr")
              assert_eq(value, "event_value")
            }
            _ => assert_true(false, "Event attribute should be preserved")
          }
        }
      }
      
      // Verify status
      assert_eq(Span::status(deserialized_span), Span::status(span))
    }
    None => assert_true(false, "Deserialized span should be available")
  }
}

// Test 2: Span Context Serialization and Deserialization
test "span context serialization and deserialization" {
  let trace_id = "0af7651916cd43dd8448eb211c80319c"
  let span_id = "b7ad6b7169203331"
  let sampled = true
  let trace_state = "test_trace_state=value"
  
  let span_ctx = SpanContext::new(trace_id, span_id, sampled, trace_state)
  
  // Serialize span context
  let serialized = SpanContext::serialize(span_ctx)
  assert_true(serialized.length() > 0, "Serialized span context should not be empty")
  
  // Deserialize span context
  let deserialized = SpanContext::deserialize(serialized)
  assert_true(deserialized != None, "Deserialized span context should not be None")
  
  match deserialized {
    Some(deserialized_ctx) => {
      // Verify span context properties
      assert_eq(SpanContext::trace_id(deserialized_ctx), trace_id)
      assert_eq(SpanContext::span_id(deserialized_ctx), span_id)
      assert_eq(SpanContext::is_sampled(deserialized_ctx), sampled)
      assert_eq(SpanContext::trace_state(deserialized_ctx), trace_state)
      assert_eq(SpanContext::is_valid(deserialized_ctx), SpanContext::is_valid(span_ctx))
    }
    None => assert_true(false, "Deserialized span context should be available")
  }
}

// Test 3: Attribute Value Serialization and Deserialization
test "attribute value serialization and deserialization" {
  // Test string attribute value
  let string_attr = StringValue("test_string_value")
  let serialized_string = AttributeValue::serialize(string_attr)
  let deserialized_string = AttributeValue::deserialize(serialized_string)
  
  match deserialized_string {
    Some(StringValue(value)) => assert_eq(value, "test_string_value"),
    _ => assert_true(false, "String attribute value should be preserved")
  }
  
  // Test int attribute value
  let int_attr = IntValue(42)
  let serialized_int = AttributeValue::serialize(int_attr)
  let deserialized_int = AttributeValue::deserialize(serialized_int)
  
  match deserialized_int {
    Some(IntValue(value)) => assert_eq(value, 42),
    _ => assert_true(false, "Int attribute value should be preserved")
  }
  
  // Test float attribute value
  let float_attr = FloatValue(3.14159265359)
  let serialized_float = AttributeValue::serialize(float_attr)
  let deserialized_float = AttributeValue::deserialize(serialized_float)
  
  match deserialized_float {
    Some(FloatValue(value)) => assert_true(abs(value - 3.14159265359) < 0.0000001),
    _ => assert_true(false, "Float attribute value should be preserved")
  }
  
  // Test bool attribute value
  let bool_attr = BoolValue(true)
  let serialized_bool = AttributeValue::serialize(bool_attr)
  let deserialized_bool = AttributeValue::deserialize(serialized_bool)
  
  match deserialized_bool {
    Some(BoolValue(value)) => assert_true(value),
    _ => assert_true(false, "Bool attribute value should be preserved")
  }
  
  // Test array string attribute value
  let array_string_attr = ArrayStringValue(["value1", "value2", "value3"])
  let serialized_array_string = AttributeValue::serialize(array_string_attr)
  let deserialized_array_string = AttributeValue::deserialize(serialized_array_string)
  
  match deserialized_array_string {
    Some(ArrayStringValue(value)) => {
      assert_eq(value.length(), 3)
      assert_eq(value[0], "value1")
      assert_eq(value[1], "value2")
      assert_eq(value[2], "value3")
    }
    _ => assert_true(false, "Array string attribute value should be preserved")
  }
  
  // Test array int attribute value
  let array_int_attr = ArrayIntValue([1, 2, 3, 4, 5])
  let serialized_array_int = AttributeValue::serialize(array_int_attr)
  let deserialized_array_int = AttributeValue::deserialize(serialized_array_int)
  
  match deserialized_array_int {
    Some(ArrayIntValue(value)) => {
      assert_eq(value.length(), 5)
      assert_eq(value[0], 1)
      assert_eq(value[1], 2)
      assert_eq(value[2], 3)
      assert_eq(value[3], 4)
      assert_eq(value[4], 5)
    }
    _ => assert_true(false, "Array int attribute value should be preserved")
  }
}

// Test 4: Log Record Serialization and Deserialization
test "log record serialization and deserialization" {
  // Create log record with attributes
  let log_attrs = [
    ("log_string_attr", StringValue("log_string_value")),
    ("log_int_attr", IntValue(123)),
    ("log_float_attr", FloatValue(4.56)),
    ("log_bool_attr", BoolValue(false))
  ]
  
  let log_record = LogRecord::new_with_attributes(
    Error,
    "Test log message for serialization",
    log_attrs
  )
  
  // Serialize log record
  let serialized = LogRecord::serialize(log_record)
  assert_true(serialized.length() > 0, "Serialized log record should not be empty")
  
  // Deserialize log record
  let deserialized = LogRecord::deserialize(serialized)
  assert_true(deserialized != None, "Deserialized log record should not be None")
  
  match deserialized {
    Some(deserialized_log_record) => {
      // Verify severity number
      assert_eq(LogRecord::severity_number(deserialized_log_record), LogRecord::severity_number(log_record))
      
      // Verify body
      match LogRecord::body(deserialized_log_record) {
        Some(message) => assert_eq(message, "Test log message for serialization"),
        None => assert_true(false, "Log record body should be preserved")
      }
      
      // Verify attributes
      let deserialized_attrs = LogRecord::attributes(deserialized_log_record)
      assert_eq(deserialized_attrs.length(), 4)
      
      for (key, value) in deserialized_attrs {
        match key {
          "log_string_attr" => {
            match value {
              StringValue(v) => assert_eq(v, "log_string_value"),
              _ => assert_true(false, "Log string attribute should be preserved")
            }
          }
          "log_int_attr" => {
            match value {
              IntValue(v) => assert_eq(v, 123),
              _ => assert_true(false, "Log int attribute should be preserved")
            }
          }
          "log_float_attr" => {
            match value {
              FloatValue(v) => assert_true(abs(v - 4.56) < 0.0001),
              _ => assert_true(false, "Log float attribute should be preserved")
            }
          }
          "log_bool_attr" => {
            match value {
              BoolValue(v) => assert_false(v),
              _ => assert_true(false, "Log bool attribute should be preserved")
            }
          }
          _ => assert_true(false, "Unexpected log attribute key")
        }
      }
    }
    None => assert_true(false, "Deserialized log record should be available")
  }
}

// Test 5: Resource Serialization and Deserialization
test "resource serialization and deserialization" {
  // Create resource with attributes
  let resource = Resource::new()
  let attrs = [
    ("service.name", StringValue("test_service")),
    ("service.version", StringValue("1.0.0")),
    ("service.instance.id", StringValue("instance-123")),
    ("host.name", StringValue("test-host")),
    ("os.type", StringValue("linux"))
  ]
  
  let resource_with_attrs = Resource::with_attributes(resource, attrs)
  
  // Serialize resource
  let serialized = Resource::serialize(resource_with_attrs)
  assert_true(serialized.length() > 0, "Serialized resource should not be empty")
  
  // Deserialize resource
  let deserialized = Resource::deserialize(serialized)
  assert_true(deserialized != None, "Deserialized resource should not be None")
  
  match deserialized {
    Some(deserialized_resource) => {
      // Verify resource attributes
      for (key, expected_value) in attrs {
        let actual_value = Resource::get_attribute(deserialized_resource, key)
        match actual_value {
          Some(value) => assert_eq(value, expected_value),
          None => assert_true(false, "Resource attribute should be preserved: " + key)
        }
      }
    }
    None => assert_true(false, "Deserialized resource should be available")
  }
}

// Test 6: Baggage Serialization and Deserialization
test "baggage serialization and deserialization" {
  let baggage = Baggage::new()
  
  // Set multiple baggage entries
  let baggage1 = Baggage::set_entry(baggage, "key1", "value1")
  let baggage2 = Baggage::set_entry(baggage1, "key2", "value2")
  let baggage3 = Baggage::set_entry(baggage2, "key3", "value3")
  
  // Serialize baggage
  let serialized = Baggage::serialize(baggage3)
  assert_true(serialized.length() > 0, "Serialized baggage should not be empty")
  
  // Deserialize baggage
  let deserialized = Baggage::deserialize(serialized)
  assert_true(deserialized != None, "Deserialized baggage should not be None")
  
  match deserialized {
    Some(deserialized_baggage) => {
      // Verify baggage entries
      let value1 = Baggage::get_entry(deserialized_baggage, "key1")
      match value1 {
        Some(v) => assert_eq(v, "value1"),
        None => assert_true(false, "Baggage entry should be preserved: key1")
      }
      
      let value2 = Baggage::get_entry(deserialized_baggage, "key2")
      match value2 {
        Some(v) => assert_eq(v, "value2"),
        None => assert_true(false, "Baggage entry should be preserved: key2")
      }
      
      let value3 = Baggage::get_entry(deserialized_baggage, "key3")
      match value3 {
        Some(v) => assert_eq(v, "value3"),
        None => assert_true(false, "Baggage entry should be preserved: key3")
      }
    }
    None => assert_true(false, "Deserialized baggage should be available")
  }
}

// Test 7: Metrics Data Serialization and Deserialization
test "metrics data serialization and deserialization" {
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "serialization_meter")
  
  // Test counter data
  let counter = Meter::create_counter(meter, "test_counter", Some("Test counter"), Some("count"))
  Counter::add(counter, 1.0)
  Counter::add(counter, 2.5)
  Counter::add(counter, 3.7)
  
  let counter_data = Counter::get_data(counter)
  
  // Serialize counter data
  let serialized_counter = MetricData::serialize_counter(counter_data)
  assert_true(serialized_counter.length() > 0, "Serialized counter data should not be empty")
  
  // Deserialize counter data
  let deserialized_counter = MetricData::deserialize_counter(serialized_counter)
  assert_true(deserialized_counter != None, "Deserialized counter data should not be None")
  
  match deserialized_counter {
    Some(data) => {
      assert_eq(data.name, counter_data.name)
      assert_eq(data.description, counter_data.description)
      assert_eq(data.unit, counter_data.unit)
      assert_true(abs(data.value - counter_data.value) < 0.0001)
    }
    None => assert_true(false, "Deserialized counter data should be available")
  }
  
  // Test histogram data
  let histogram = Meter::create_histogram(meter, "test_histogram", Some("Test histogram"), Some("ms"))
  Histogram::record(histogram, 100.0)
  Histogram::record(histogram, 200.0)
  Histogram::record(histogram, 150.0)
  
  let histogram_data = Histogram::get_data(histogram)
  
  // Serialize histogram data
  let serialized_histogram = MetricData::serialize_histogram(histogram_data)
  assert_true(serialized_histogram.length() > 0, "Serialized histogram data should not be empty")
  
  // Deserialize histogram data
  let deserialized_histogram = MetricData::deserialize_histogram(serialized_histogram)
  assert_true(deserialized_histogram != None, "Deserialized histogram data should not be None")
  
  match deserialized_histogram {
    Some(data) => {
      assert_eq(data.name, histogram_data.name)
      assert_eq(data.description, histogram_data.description)
      assert_eq(data.unit, histogram_data.unit)
      assert_eq(data.count, histogram_data.count)
      assert_true(abs(data.sum - histogram_data.sum) < 0.0001)
      assert_eq(data.min, histogram_data.min)
      assert_eq(data.max, histogram_data.max)
    }
    None => assert_true(false, "Deserialized histogram data should be available")
  }
}

// Test 8: Cross-Format Serialization
test "cross-format serialization" {
  let provider = TracerProvider::default()
  let tracer = TracerProvider::get_tracer(provider, "cross_format_tracer")
  let span = Tracer::start_span(tracer, "cross_format_span")
  
  // Add attributes
  Span::set_attribute(span, "string_attr", StringValue("string_value"))
  Span::set_attribute(span, "int_attr", IntValue(42))
  
  // Add event
  Span::add_event(span, "cross_format_event", None)
  
  // End span
  Span::end(span)
  
  // Serialize to JSON
  let json_serialized = Span::serialize_to_json(span)
  assert_true(json_serialized.length() > 0, "JSON serialized span should not be empty")
  
  // Deserialize from JSON
  let json_deserialized = Span::deserialize_from_json(json_serialized)
  assert_true(json_deserialized != None, "Deserialized span from JSON should not be None")
  
  match json_deserialized {
    Some(deserialized_span) => {
      // Verify span name
      assert_eq(Span::name(deserialized_span), Span::name(span))
      
      // Verify attributes
      let string_attr = Span::get_attribute(deserialized_span, "string_attr")
      match string_attr {
        Some(StringValue(value)) => assert_eq(value, "string_value"),
        _ => assert_true(false, "String attribute should be preserved in JSON serialization")
      }
      
      let int_attr = Span::get_attribute(deserialized_span, "int_attr")
      match int_attr {
        Some(IntValue(value)) => assert_eq(value, 42),
        _ => assert_true(false, "Int attribute should be preserved in JSON serialization")
      }
      
      // Verify events
      let events = Span::get_events(deserialized_span)
      assert_eq(events.length(), 1)
      match events[0] {
        Event(name, attrs) => {
          assert_eq(name, "cross_format_event")
          assert_eq(attrs.length(), 0)
        }
      }
    }
    None => assert_true(false, "Deserialized span from JSON should be available")
  }
  
  // Serialize to Binary
  let binary_serialized = Span::serialize_to_binary(span)
  assert_true(binary_serialized.length() > 0, "Binary serialized span should not be empty")
  
  // Deserialize from Binary
  let binary_deserialized = Span::deserialize_from_binary(binary_serialized)
  assert_true(binary_deserialized != None, "Deserialized span from Binary should not be None")
  
  match binary_deserialized {
    Some(deserialized_span) => {
      // Verify span name
      assert_eq(Span::name(deserialized_span), Span::name(span))
      
      // Verify attributes
      let string_attr = Span::get_attribute(deserialized_span, "string_attr")
      match string_attr {
        Some(StringValue(value)) => assert_eq(value, "string_value"),
        _ => assert_true(false, "String attribute should be preserved in binary serialization")
      }
      
      let int_attr = Span::get_attribute(deserialized_span, "int_attr")
      match int_attr {
        Some(IntValue(value)) => assert_eq(value, 42),
        _ => assert_true(false, "Int attribute should be preserved in binary serialization")
      }
      
      // Verify events
      let events = Span::get_events(deserialized_span)
      assert_eq(events.length(), 1)
      match events[0] {
        Event(name, attrs) => {
          assert_eq(name, "cross_format_event")
          assert_eq(attrs.length(), 0)
        }
      }
    }
    None => assert_true(false, "Deserialized span from Binary should be available")
  }
}

// Test 9: Serialization Error Handling
test "serialization error handling" {
  // Test with invalid data
  let invalid_data = "invalid_serialized_data"
  
  // Try to deserialize span
  let deserialized_span = Span::deserialize(invalid_data)
  assert_true(deserialized_span == None, "Deserialization of invalid data should return None")
  
  // Try to deserialize span context
  let deserialized_ctx = SpanContext::deserialize(invalid_data)
  assert_true(deserialized_ctx == None, "Deserialization of invalid data should return None")
  
  // Try to deserialize attribute value
  let deserialized_attr = AttributeValue::deserialize(invalid_data)
  assert_true(deserialized_attr == None, "Deserialization of invalid data should return None")
  
  // Try to deserialize log record
  let deserialized_log = LogRecord::deserialize(invalid_data)
  assert_true(deserialized_log == None, "Deserialization of invalid data should return None")
  
  // Try to deserialize resource
  let deserialized_resource = Resource::deserialize(invalid_data)
  assert_true(deserialized_resource == None, "Deserialization of invalid data should return None")
  
  // Try to deserialize baggage
  let deserialized_baggage = Baggage::deserialize(invalid_data)
  assert_true(deserialized_baggage == None, "Deserialization of invalid data should return None")
}

// Test 10: Large Data Serialization
test "large data serialization" {
  let provider = TracerProvider::default()
  let tracer = TracerProvider::get_tracer(provider, "large_data_tracer")
  let span = Tracer::start_span(tracer, "large_data_span")
  
  // Add large number of attributes
  for i in 0..=999 {
    let key = "large_data_key_" + i.to_string()
    let value = StringValue("large_data_value_" + i.to_string())
    Span::set_attribute(span, key, value)
  }
  
  // Add large number of events
  for i in 0..=499 {
    let event_name = "large_data_event_" + i.to_string()
    Span::add_event(span, event_name, None)
  }
  
  // End span
  Span::end(span)
  
  // Serialize span
  let serialized = Span::serialize(span)
  assert_true(serialized.length() > 0, "Serialized large span should not be empty")
  
  // Deserialize span
  let deserialized = Span::deserialize(serialized)
  assert_true(deserialized != None, "Deserialized large span should not be None")
  
  match deserialized {
    Some(deserialized_span) => {
      // Verify all attributes are preserved
      for i in 0..=999 {
        let key = "large_data_key_" + i.to_string()
        let expected_value = StringValue("large_data_value_" + i.to_string())
        
        let actual_value = Span::get_attribute(deserialized_span, key)
        match actual_value {
          Some(value) => assert_eq(value, expected_value),
          None => assert_true(false, "Large data attribute should be preserved: " + key)
        }
      }
      
      // Verify all events are preserved
      let events = Span::get_events(deserialized_span)
      assert_eq(events.length(), 500)
      
      for i in 0..=499 {
        let expected_event_name = "large_data_event_" + i.to_string()
        match events[i] {
          Event(name, attrs) => {
            assert_eq(name, expected_event_name)
            assert_eq(attrs.length(), 0)
          }
        }
      }
    }
    None => assert_true(false, "Deserialized large span should be available")
  }
}

// Helper function to calculate absolute value
fn abs(x : Float) -> Float {
  if x < 0.0 {
    -x
  } else {
    x
  }
}