// Azimuth Telemetry System - Data Integrity Validation Tests
// This file contains comprehensive data integrity validation test cases for the telemetry system

// Test 1: Attribute Data Integrity
test "attribute data integrity" {
  let provider = TracerProvider::default()
  let tracer = TracerProvider::get_tracer(provider, "integrity_test_tracer")
  let span = Tracer::start_span(tracer, "attribute_integrity_span")
  
  // Test string attribute integrity
  let original_string = "test_string_value_12345"
  Span::set_attribute(span, "string_key", StringValue(original_string))
  
  let retrieved_string = Span::get_attribute(span, "string_key")
  match retrieved_string {
    Some(StringValue(value)) => assert_eq(value, original_string),
    _ => assert_true(false, "String attribute should maintain integrity")
  }
  
  // Test int attribute integrity
  let original_int = 42
  Span::set_attribute(span, "int_key", IntValue(original_int))
  
  let retrieved_int = Span::get_attribute(span, "int_key")
  match retrieved_int {
    Some(IntValue(value)) => assert_eq(value, original_int),
    _ => assert_true(false, "Int attribute should maintain integrity")
  }
  
  // Test float attribute integrity
  let original_float = 3.14159265359
  Span::set_attribute(span, "float_key", FloatValue(original_float))
  
  let retrieved_float = Span::get_attribute(span, "float_key")
  match retrieved_float {
    Some(FloatValue(value)) => assert_true(abs(value - original_float) < 0.0000001),
    _ => assert_true(false, "Float attribute should maintain integrity")
  }
  
  // Test bool attribute integrity
  let original_bool = true
  Span::set_attribute(span, "bool_key", BoolValue(original_bool))
  
  let retrieved_bool = Span::get_attribute(span, "bool_key")
  match retrieved_bool {
    Some(BoolValue(value)) => assert_eq(value, original_bool),
    _ => assert_true(false, "Bool attribute should maintain integrity")
  }
  
  // Test array string attribute integrity
  let original_array_string = ["value1", "value2", "value3", "value4", "value5"]
  Span::set_attribute(span, "array_string_key", ArrayStringValue(original_array_string))
  
  let retrieved_array_string = Span::get_attribute(span, "array_string_key")
  match retrieved_array_string {
    Some(ArrayStringValue(value)) => {
      assert_eq(value.length(), original_array_string.length())
      for i in 0..=original_array_string.length() - 1 {
        assert_eq(value[i], original_array_string[i])
      }
    }
    _ => assert_true(false, "Array string attribute should maintain integrity")
  }
  
  // Test array int attribute integrity
  let original_array_int = [1, 2, 3, 4, 5]
  Span::set_attribute(span, "array_int_key", ArrayIntValue(original_array_int))
  
  let retrieved_array_int = Span::get_attribute(span, "array_int_key")
  match retrieved_array_int {
    Some(ArrayIntValue(value)) => {
      assert_eq(value.length(), original_array_int.length())
      for i in 0..=original_array_int.length() - 1 {
        assert_eq(value[i], original_array_int[i])
      }
    }
    _ => assert_true(false, "Array int attribute should maintain integrity")
  }
  
  Span::end(span)
}

// Test 2: Span Context Data Integrity
test "span context data integrity" {
  let original_trace_id = "0af7651916cd43dd8448eb211c80319c"
  let original_span_id = "b7ad6b7169203331"
  let original_sampled = true
  let original_trace_state = "test_trace_state=value"
  
  let span_ctx = SpanContext::new(original_trace_id, original_span_id, original_sampled, original_trace_state)
  
  // Verify trace ID integrity
  assert_eq(SpanContext::trace_id(span_ctx), original_trace_id)
  
  // Verify span ID integrity
  assert_eq(SpanContext::span_id(span_ctx), original_span_id)
  
  // Verify sampling decision integrity
  assert_eq(SpanContext::is_sampled(span_ctx), original_sampled)
  
  // Verify trace state integrity
  assert_eq(SpanContext::trace_state(span_ctx), original_trace_state)
  
  // Test with span
  let provider = TracerProvider::default()
  let tracer = TracerProvider::get_tracer(provider, "context_integrity_tracer")
  let span = Tracer::start_span_with_context(tracer, "context_integrity_span", span_ctx)
  
  match span {
    Some(s) => {
      let retrieved_ctx = Span::span_context(s)
      assert_eq(SpanContext::trace_id(retrieved_ctx), original_trace_id)
      assert_eq(SpanContext::span_id(retrieved_ctx), original_span_id)
      assert_eq(SpanContext::is_sampled(retrieved_ctx), original_sampled)
      assert_eq(SpanContext::trace_state(retrieved_ctx), original_trace_state)
    }
    None => assert_true(false, "Should be able to create span with context")
  }
}

// Test 3: Event Data Integrity
test "event data integrity" {
  let provider = TracerProvider::default()
  let tracer = TracerProvider::get_tracer(provider, "event_integrity_tracer")
  let span = Tracer::start_span(tracer, "event_integrity_span")
  
  // Test simple event integrity
  let original_event_name = "test_event_name"
  Span::add_event(span, original_event_name, None)
  
  // Test event with attributes integrity
  let event_attrs = [
    ("event_string_attr", StringValue("event_string_value")),
    ("event_int_attr", IntValue(123)),
    ("event_float_attr", FloatValue(4.56)),
    ("event_bool_attr", BoolValue(true))
  ]
  
  let original_complex_event_name = "complex_test_event"
  Span::add_event(span, original_complex_event_name, Some(event_attrs))
  
  // Verify events are stored with integrity
  let events = Span::get_events(span)
  assert_eq(events.length(), 2)
  
  // Verify first event
  match events[0] {
    Event(name, attrs) => {
      assert_eq(name, original_event_name)
      assert_eq(attrs.length(), 0)
    }
  }
  
  // Verify second event
  match events[1] {
    Event(name, attrs) => {
      assert_eq(name, original_complex_event_name)
      assert_eq(attrs.length(), 4)
      
      // Verify event attributes
      for (key, value) in attrs {
        match key {
          "event_string_attr" => {
            match value {
              StringValue(v) => assert_eq(v, "event_string_value"),
              _ => assert_true(false, "Event string attribute should maintain integrity")
            }
          }
          "event_int_attr" => {
            match value {
              IntValue(v) => assert_eq(v, 123),
              _ => assert_true(false, "Event int attribute should maintain integrity")
            }
          }
          "event_float_attr" => {
            match value {
              FloatValue(v) => assert_true(abs(v - 4.56) < 0.0001),
              _ => assert_true(false, "Event float attribute should maintain integrity")
            }
          }
          "event_bool_attr" => {
            match value {
              BoolValue(v) => assert_true(v),
              _ => assert_true(false, "Event bool attribute should maintain integrity")
            }
          }
          _ => assert_true(false, "Unexpected event attribute key")
        }
      }
    }
  }
  
  Span::end(span)
}

// Test 4: Metrics Data Integrity
test "metrics data integrity" {
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "metrics_integrity_meter")
  
  // Test counter data integrity
  let counter = Meter::create_counter(meter, "test_counter", Some("Test counter"), Some("count"))
  
  // Add multiple values to counter
  Counter::add(counter, 1.0)
  Counter::add(counter, 2.5)
  Counter::add(counter, 3.7)
  
  // Verify counter data integrity
  let counter_data = Counter::get_data(counter)
  assert_eq(counter_data.name, "test_counter")
  assert_eq(counter_data.description, Some("Test counter"))
  assert_eq(counter_data.unit, Some("count"))
  assert_true(abs(counter_data.value - 7.2) < 0.0001) // 1.0 + 2.5 + 3.7 = 7.2
  
  // Test histogram data integrity
  let histogram = Meter::create_histogram(meter, "test_histogram", Some("Test histogram"), Some("ms"))
  
  // Record multiple values to histogram
  Histogram::record(histogram, 100.0)
  Histogram::record(histogram, 200.0)
  Histogram::record(histogram, 150.0)
  Histogram::record(histogram, 300.0)
  
  // Verify histogram data integrity
  let histogram_data = Histogram::get_data(histogram)
  assert_eq(histogram_data.name, "test_histogram")
  assert_eq(histogram_data.description, Some("Test histogram"))
  assert_eq(histogram_data.unit, Some("ms"))
  assert_eq(histogram_data.count, 4)
  assert_eq(histogram_data.sum, 750.0)
  assert_eq(histogram_data.min, 100.0)
  assert_eq(histogram_data.max, 300.0)
  
  // Test updown counter data integrity
  let updown_counter = Meter::create_updown_counter(meter, "test_updown", Some("Test updown"), Some("value"))
  
  // Add positive and negative values
  UpDownCounter::add(updown_counter, 10.0)
  UpDownCounter::add(updown_counter, -3.0)
  UpDownCounter::add(updown_counter, 5.0)
  UpDownCounter::add(updown_counter, -2.0)
  
  // Verify updown counter data integrity
  let updown_data = UpDownCounter::get_data(updown_counter)
  assert_eq(updown_data.name, "test_updown")
  assert_eq(updown_data.description, Some("Test updown"))
  assert_eq(updown_data.unit, Some("value"))
  assert_true(abs(updown_data.value - 10.0) < 0.0001) // 10.0 - 3.0 + 5.0 - 2.0 = 10.0
}

// Test 5: Log Record Data Integrity
test "log record data integrity" {
  let provider = LoggerProvider::default()
  let logger = LoggerProvider::get_logger(provider, "log_integrity_logger")
  
  // Test basic log record integrity
  let original_message = "Test log message for integrity"
  let log_record = LogRecord::new(Info, original_message)
  
  assert_eq(LogRecord::severity_number(log_record), Info)
  match LogRecord::body(log_record) {
    Some(message) => assert_eq(message, original_message),
    None => assert_true(false, "Log record body should maintain integrity")
  }
  
  // Test log record with attributes integrity
  let log_attrs = [
    ("log_string_attr", StringValue("log_string_value")),
    ("log_int_attr", IntValue(456)),
    ("log_float_attr", FloatValue(7.89)),
    ("log_bool_attr", BoolValue(false))
  ]
  
  let original_detailed_message = "Detailed log message for integrity"
  let detailed_log_record = LogRecord::new_with_attributes(
    Error,
    original_detailed_message,
    log_attrs
  )
  
  assert_eq(LogRecord::severity_number(detailed_log_record), Error)
  match LogRecord::body(detailed_log_record) {
    Some(message) => assert_eq(message, original_detailed_message),
    None => assert_true(false, "Detailed log record body should maintain integrity")
  }
  
  let detailed_attrs = LogRecord::attributes(detailed_log_record)
  assert_eq(detailed_attrs.length(), 4)
  
  // Verify log attributes
  for (key, value) in detailed_attrs {
    match key {
      "log_string_attr" => {
        match value {
          StringValue(v) => assert_eq(v, "log_string_value"),
          _ => assert_true(false, "Log string attribute should maintain integrity")
        }
      }
      "log_int_attr" => {
        match value {
          IntValue(v) => assert_eq(v, 456),
          _ => assert_true(false, "Log int attribute should maintain integrity")
        }
      }
      "log_float_attr" => {
        match value {
          FloatValue(v) => assert_true(abs(v - 7.89) < 0.0001),
          _ => assert_true(false, "Log float attribute should maintain integrity")
        }
      }
      "log_bool_attr" => {
        match value {
          BoolValue(v) => assert_false(v),
          _ => assert_true(false, "Log bool attribute should maintain integrity")
        }
      }
      _ => assert_true(false, "Unexpected log attribute key")
    }
  }
  
  // Emit log records
  Logger::emit(logger, log_record)
  Logger::emit(logger, detailed_log_record)
}

// Test 6: Context Propagation Data Integrity
test "context propagation data integrity" {
  let root_ctx = Context::root()
  
  // Create context with multiple values
  let key1 = ContextKey::new("key1")
  let value1 = "value1"
  let ctx1 = Context::with_value(root_ctx, key1, value1)
  
  let key2 = ContextKey::new("key2")
  let value2 = "value2"
  let ctx2 = Context::with_value(ctx1, key2, value2)
  
  let key3 = ContextKey::new("key3")
  let value3 = "value3"
  let ctx3 = Context::with_value(ctx2, key3, value3)
  
  // Verify context propagation integrity
  match Context::get(ctx3, key1) {
    Some(v) => assert_eq(v, value1),
    None => assert_true(false, "Context should maintain integrity for key1")
  }
  
  match Context::get(ctx3, key2) {
    Some(v) => assert_eq(v, value2),
    None => assert_true(false, "Context should maintain integrity for key2")
  }
  
  match Context::get(ctx3, key3) {
    Some(v) => assert_eq(v, value3),
    None => assert_true(false, "Context should maintain integrity for key3")
  }
  
  // Verify non-existent key returns None
  let non_existent_key = ContextKey::new("non_existent_key")
  match Context::get(ctx3, non_existent_key) {
    Some(_) => assert_true(false, "Non-existent key should return None"),
    None => assert_true(true, "Non-existent key should return None")
  }
}

// Test 7: Baggage Data Integrity
test "baggage data integrity" {
  let baggage = Baggage::new()
  
  // Set multiple baggage entries
  let updated_baggage1 = Baggage::set_entry(baggage, "key1", "value1")
  let updated_baggage2 = Baggage::set_entry(updated_baggage1, "key2", "value2")
  let updated_baggage3 = Baggage::set_entry(updated_baggage2, "key3", "value3")
  
  // Verify baggage integrity
  let value1 = Baggage::get_entry(updated_baggage3, "key1")
  match value1 {
    Some(v) => assert_eq(v, "value1"),
    None => assert_true(false, "Baggage should maintain integrity for key1")
  }
  
  let value2 = Baggage::get_entry(updated_baggage3, "key2")
  match value2 {
    Some(v) => assert_eq(v, "value2"),
    None => assert_true(false, "Baggage should maintain integrity for key2")
  }
  
  let value3 = Baggage::get_entry(updated_baggage3, "key3")
  match value3 {
    Some(v) => assert_eq(v, "value3"),
    None => assert_true(false, "Baggage should maintain integrity for key3")
  }
  
  // Test baggage removal integrity
  let baggage_after_removal = Baggage::remove_entry(updated_baggage3, "key2")
  
  let value1_after_removal = Baggage::get_entry(baggage_after_removal, "key1")
  match value1_after_removal {
    Some(v) => assert_eq(v, "value1"),
    None => assert_true(false, "Baggage should maintain integrity for key1 after removal")
  }
  
  let value2_after_removal = Baggage::get_entry(baggage_after_removal, "key2")
  match value2_after_removal {
    Some(_) => assert_true(false, "Removed key should not be accessible"),
    None => assert_true(true, "Removed key should not be accessible")
  }
  
  let value3_after_removal = Baggage::get_entry(baggage_after_removal, "key3")
  match value3_after_removal {
    Some(v) => assert_eq(v, "value3"),
    None => assert_true(false, "Baggage should maintain integrity for key3 after removal")
  }
}

// Test 8: Resource Data Integrity
test "resource data integrity" {
  let resource = Resource::new()
  
  // Set multiple resource attributes
  let attrs = [
    ("service.name", StringValue("test_service")),
    ("service.version", StringValue("1.0.0")),
    ("service.instance.id", StringValue("instance-123")),
    ("host.name", StringValue("test-host")),
    ("os.type", StringValue("linux"))
  ]
  
  let resource_with_attrs = Resource::with_attributes(resource, attrs)
  
  // Verify resource attributes integrity
  for (key, expected_value) in attrs {
    let actual_value = Resource::get_attribute(resource_with_attrs, key)
    match actual_value {
      Some(value) => assert_eq(value, expected_value),
      None => assert_true(false, "Resource should maintain integrity for key: " + key)
    }
  }
  
  // Test resource merging integrity
  let override_attrs = [
    ("service.version", StringValue("2.0.0")), // Override existing
    ("deployment.environment", StringValue("production")) // New attribute
  ]
  
  let override_resource = Resource::with_attributes(Resource::new(), override_attrs)
  let merged_resource = Resource::merge(resource_with_attrs, override_resource)
  
  // Verify merged resource integrity
  let service_name = Resource::get_attribute(merged_resource, "service.name")
  match service_name {
    Some(StringValue(name)) => assert_eq(name, "test_service"),
    _ => assert_true(false, "Merged resource should maintain original service.name")
  }
  
  let service_version = Resource::get_attribute(merged_resource, "service.version")
  match service_version {
    Some(StringValue(version)) => assert_eq(version, "2.0.0"),
    _ => assert_true(false, "Merged resource should override service.version")
  }
  
  let deployment_env = Resource::get_attribute(merged_resource, "deployment.environment")
  match deployment_env {
    Some(StringValue(env)) => assert_eq(env, "production"),
    _ => assert_true(false, "Merged resource should include new deployment.environment")
  }
}

// Test 9: Large Data Set Integrity
test "large data set integrity" {
  let provider = TracerProvider::default()
  let tracer = TracerProvider::get_tracer(provider, "large_data_tracer")
  let span = Tracer::start_span(tracer, "large_data_span")
  
  // Test with large number of attributes
  for i in 0..=999 {
    let key = "large_data_key_" + i.to_string()
    let value = StringValue("large_data_value_" + i.to_string())
    Span::set_attribute(span, key, value)
  }
  
  // Verify all attributes are stored with integrity
  for i in 0..=999 {
    let key = "large_data_key_" + i.to_string()
    let expected_value = StringValue("large_data_value_" + i.to_string())
    
    let actual_value = Span::get_attribute(span, key)
    match actual_value {
      Some(value) => assert_eq(value, expected_value),
      None => assert_true(false, "Large data set should maintain integrity for key: " + key)
    }
  }
  
  // Test with large number of events
  for i in 0..=499 {
    let event_name = "large_data_event_" + i.to_string()
    Span::add_event(span, event_name, None)
  }
  
  // Verify all events are stored with integrity
  let events = Span::get_events(span)
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
  
  Span::end(span)
}

// Test 10: Data Corruption Detection
test "data corruption detection" {
  let provider = TracerProvider::default()
  let tracer = TracerProvider::get_tracer(provider, "corruption_detection_tracer")
  let span = Tracer::start_span(tracer, "corruption_detection_span")
  
  // Set known values
  let known_string_value = "known_string_value"
  let known_int_value = 42
  let known_float_value = 3.14159
  let known_bool_value = true
  
  Span::set_attribute(span, "known_string", StringValue(known_string_value))
  Span::set_attribute(span, "known_int", IntValue(known_int_value))
  Span::set_attribute(span, "known_float", FloatValue(known_float_value))
  Span::set_attribute(span, "known_bool", BoolValue(known_bool_value))
  
  // Verify values are correct
  let string_value = Span::get_attribute(span, "known_string")
  match string_value {
    Some(StringValue(value)) => assert_eq(value, known_string_value),
    _ => assert_true(false, "String value should not be corrupted")
  }
  
  let int_value = Span::get_attribute(span, "known_int")
  match int_value {
    Some(IntValue(value)) => assert_eq(value, known_int_value),
    _ => assert_true(false, "Int value should not be corrupted")
  }
  
  let float_value = Span::get_attribute(span, "known_float")
  match float_value {
    Some(FloatValue(value)) => assert_true(abs(value - known_float_value) < 0.0001),
    _ => assert_true(false, "Float value should not be corrupted")
  }
  
  let bool_value = Span::get_attribute(span, "known_bool")
  match bool_value {
    Some(BoolValue(value)) => assert_eq(value, known_bool_value),
    _ => assert_true(false, "Bool value should not be corrupted")
  }
  
  // Test data integrity checksum if available
  let checksum = Span::calculate_checksum(span)
  assert_true(checksum != 0, "Checksum should be non-zero for valid data")
  
  // Modify span and verify checksum changes
  Span::set_attribute(span, "new_attribute", StringValue("new_value"))
  let new_checksum = Span::calculate_checksum(span)
  assert_true(checksum != new_checksum, "Checksum should change when data is modified")
  
  Span::end(span)
}

// Helper function to calculate absolute value
fn abs(x : Float) -> Float {
  if x < 0.0 {
    -x
  } else {
    x
  }
}