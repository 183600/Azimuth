// Azimuth Telemetry System - Comprehensive Serialization Tests
// This file contains comprehensive test cases for telemetry data serialization

// Test 1: Basic Attribute Value Serialization
test "basic attribute value serialization" {
  // Test string serialization
  let string_attr = StringValue("test_value")
  let serialized_string = AttributeSerializer::serialize(string_attr)
  let deserialized_string = AttributeSerializer::deserialize_string(serialized_string)
  match deserialized_string {
    StringValue(v) => assert_eq(v, "test_value")
    _ => assert_true(false)
  }
  
  // Test int serialization
  let int_attr = IntValue(42)
  let serialized_int = AttributeSerializer::serialize(int_attr)
  let deserialized_int = AttributeSerializer::deserialize_int(serialized_int)
  match deserialized_int {
    IntValue(v) => assert_eq(v, 42)
    _ => assert_true(false)
  }
  
  // Test float serialization
  let float_attr = FloatValue(3.14)
  let serialized_float = AttributeSerializer::serialize(float_attr)
  let deserialized_float = AttributeSerializer::deserialize_float(serialized_float)
  match deserialized_float {
    FloatValue(v) => assert_eq(v, 3.14)
    _ => assert_true(false)
  }
  
  // Test bool serialization
  let bool_attr = BoolValue(true)
  let serialized_bool = AttributeSerializer::serialize(bool_attr)
  let deserialized_bool = AttributeSerializer::deserialize_bool(serialized_bool)
  match deserialized_bool {
    BoolValue(v) => assert_true(v)
    _ => assert_true(false)
  }
}

// Test 2: Array Attribute Value Serialization
test "array attribute value serialization" {
  // Test array string serialization
  let array_string_attr = ArrayStringValue(["a", "b", "c"])
  let serialized_array_string = AttributeSerializer::serialize(array_string_attr)
  let deserialized_array_string = AttributeSerializer::deserialize_array_string(serialized_array_string)
  match deserialized_array_string {
    ArrayStringValue(v) => {
      assert_eq(v.length(), 3)
      assert_eq(v[0], "a")
      assert_eq(v[1], "b")
      assert_eq(v[2], "c")
    }
    _ => assert_true(false)
  }
  
  // Test array int serialization
  let array_int_attr = ArrayIntValue([1, 2, 3])
  let serialized_array_int = AttributeSerializer::serialize(array_int_attr)
  let deserialized_array_int = AttributeSerializer::deserialize_array_int(serialized_array_int)
  match deserialized_array_int {
    ArrayIntValue(v) => {
      assert_eq(v.length(), 3)
      assert_eq(v[0], 1)
      assert_eq(v[1], 2)
      assert_eq(v[2], 3)
    }
    _ => assert_true(false)
  }
}

// Test 3: Attributes Map Serialization
test "attributes map serialization" {
  let attrs = Attributes::new()
  
  // Set various attribute types
  Attributes::set(attrs, "string.key", StringValue("test_value"))
  Attributes::set(attrs, "int.key", IntValue(42))
  Attributes::set(attrs, "float.key", FloatValue(3.14))
  Attributes::set(attrs, "bool.key", BoolValue(true))
  
  // Serialize attributes
  let serialized_attrs = AttributesSerializer::serialize(attrs)
  
  // Deserialize attributes
  let deserialized_attrs = AttributesSerializer::deserialize(serialized_attrs)
  
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
}

// Test 4: Span Context Serialization
test "span context serialization" {
  let trace_id = "0af7651916cd43dd8448eb211c80319c"
  let span_id = "b7ad6b7169203331"
  let span_ctx = SpanContext::new(trace_id, span_id, true, "test_state")
  
  // Serialize span context
  let serialized_ctx = SpanContextSerializer::serialize(span_ctx)
  
  // Deserialize span context
  let deserialized_ctx = SpanContextSerializer::deserialize(serialized_ctx)
  
  // Verify deserialized span context
  assert_eq(SpanContext::trace_id(deserialized_ctx), trace_id)
  assert_eq(SpanContext::span_id(deserialized_ctx), span_id)
  assert_true(SpanContext::is_valid(deserialized_ctx))
  assert_true(SpanContext::is_sampled(deserialized_ctx))
}

// Test 5: Span Serialization
test "span serialization" {
  let span_ctx = SpanContext::new("trace_id", "span_id", true, "")
  let span = Span::new("test_span", Internal, span_ctx)
  
  // Add some data to the span
  Span::set_status(span, Ok, Some("Operation completed"))
  Span::add_event(span, "test_event", Some([("test_attr", StringValue("test_value"))]))
  
  // Serialize span
  let serialized_span = SpanSerializer::serialize(span)
  
  // Deserialize span
  let deserialized_span = SpanSerializer::deserialize(serialized_span)
  
  // Verify deserialized span
  assert_eq(Span::name(deserialized_span), "test_span")
  match Span::kind(deserialized_span) {
    Internal => assert_true(true)
    _ => assert_true(false)
  }
  assert_eq(Span::status(deserialized_span), Ok)
  
  let retrieved_ctx = Span::span_context(deserialized_span)
  assert_eq(SpanContext::trace_id(retrieved_ctx), "trace_id")
  assert_eq(SpanContext::span_id(retrieved_ctx), "span_id")
}

// Test 6: Log Record Serialization
test "log record serialization" {
  let attrs = Attributes::new()
  Attributes::set(attrs, "log.level", StringValue("info"))
  
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
  let serialized_log = LogRecordSerializer::serialize(log_record)
  
  // Deserialize log record
  let deserialized_log = LogRecordSerializer::deserialize(serialized_log)
  
  // Verify deserialized log record
  assert_eq(LogRecord::severity_number(deserialized_log), Info)
  match LogRecord::body(deserialized_log) {
    Some(body) => assert_eq(body, "Test log message")
    None => assert_true(false)
  }
  assert_eq(LogRecord::trace_id(deserialized_log), Some("trace_id"))
  assert_eq(LogRecord::span_id(deserialized_log), Some("span_id"))
}

// Test 7: Resource Serialization
test "resource serialization" {
  let attrs = [
    ("service.name", StringValue("test_service")),
    ("service.version", StringValue("1.0.0")),
    ("service.instance.id", StringValue("instance-123"))
  ]
  let resource = Resource::with_attributes(Resource::new(), attrs)
  
  // Serialize resource
  let serialized_resource = ResourceSerializer::serialize(resource)
  
  // Deserialize resource
  let deserialized_resource = ResourceSerializer::deserialize(serialized_resource)
  
  // Verify deserialized resource
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
}

// Test 8: Metrics Serialization
test "metrics serialization" {
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "test_meter")
  
  // Create and configure a counter
  let counter = Meter::create_counter(meter, "test_counter", Some("Test counter"), Some("count"))
  Counter::add(counter, 1.0)
  
  // Serialize metrics
  let serialized_metrics = MetricsSerializer::serialize(counter)
  
  // Deserialize metrics
  let deserialized_counter = MetricsSerializer::deserialize_counter(serialized_metrics)
  
  // Verify deserialized metrics
  let instrument = Histogram::as_instrument(deserialized_counter)
  assert_eq(Instrument::name(instrument), "test_counter")
  assert_eq(Instrument::description(instrument), Some("Test counter"))
  assert_eq(Instrument::unit(instrument), Some("count"))
}

// Test 9: JSON Format Serialization
test "json format serialization" {
  let attrs = Attributes::new()
  Attributes::set(attrs, "string.key", StringValue("test_value"))
  Attributes::set(attrs, "int.key", IntValue(42))
  
  // Serialize to JSON
  let json_data = JsonFormatter::format_attributes(attrs)
  
  // Verify JSON contains expected data
  assert_true(StringUtils::contains(json_data, "string.key"))
  assert_true(StringUtils::contains(json_data, "test_value"))
  assert_true(StringUtils::contains(json_data, "int.key"))
  assert_true(StringUtils::contains(json_data, "42"))
  
  // Parse JSON back to attributes
  let parsed_attrs = JsonParser::parse_attributes(json_data)
  
  // Verify parsed attributes
  let string_result = Attributes::get(parsed_attrs, "string.key")
  match string_result {
    Some(StringValue(v)) => assert_eq(v, "test_value")
    _ => assert_true(false)
  }
}

// Test 10: Binary Format Serialization
test "binary format serialization" {
  let span_ctx = SpanContext::new("trace_id", "span_id", true, "")
  let span = Span::new("test_span", Internal, span_ctx)
  
  // Serialize to binary format
  let binary_data = BinaryFormatter::format_span(span)
  
  // Verify binary data is not empty
  assert_true(binary_data.length() > 0)
  
  // Deserialize from binary format
  let deserialized_span = BinaryParser::parse_span(binary_data)
  
  // Verify deserialized span
  assert_eq(Span::name(deserialized_span), "test_span")
  match Span::kind(deserialized_span) {
    Internal => assert_true(true)
    _ => assert_true(false)
  }
  
  let retrieved_ctx = Span::span_context(deserialized_span)
  assert_eq(SpanContext::trace_id(retrieved_ctx), "trace_id")
  assert_eq(SpanContext::span_id(retrieved_ctx), "span_id")
}