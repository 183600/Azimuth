// Azimuth Serialization and Deserialization Tests
// This file contains test cases for data serialization and deserialization functionality

test "attribute value serialization and deserialization" {
  // Test string attribute value
  let string_attr = StringValue("test_string_value")
  let serialized_string = AttributeSerializer::serialize(string_attr)
  let deserialized_string = AttributeSerializer::deserialize(serialized_string)
  
  match deserialized_string {
    StringValue(v) => assert_eq(v, "test_string_value")
    _ => assert_true(false)
  }
  
  // Test int attribute value
  let int_attr = IntValue(42)
  let serialized_int = AttributeSerializer::serialize(int_attr)
  let deserialized_int = AttributeSerializer::deserialize(serialized_int)
  
  match deserialized_int {
    IntValue(v) => assert_eq(v, 42)
    _ => assert_true(false)
  }
  
  // Test float attribute value
  let float_attr = FloatValue(3.14159)
  let serialized_float = AttributeSerializer::serialize(float_attr)
  let deserialized_float = AttributeSerializer::deserialize(serialized_float)
  
  match deserialized_float {
    FloatValue(v) => assert_true(abs(v - 3.14159) < 0.00001)
    _ => assert_true(false)
  }
  
  // Test bool attribute value
  let bool_attr = BoolValue(true)
  let serialized_bool = AttributeSerializer::serialize(bool_attr)
  let deserialized_bool = AttributeSerializer::deserialize(serialized_bool)
  
  match deserialized_bool {
    BoolValue(v) => assert_true(v)
    _ => assert_true(false)
  }
}

test "array attribute serialization and deserialization" {
  // Test string array
  let string_array = ArrayStringValue(["a", "b", "c", "d", "e"])
  let serialized_string_array = AttributeSerializer::serialize(string_array)
  let deserialized_string_array = AttributeSerializer::deserialize(serialized_string_array)
  
  match deserialized_string_array {
    ArrayStringValue(v) => {
      assert_eq(v.length(), 5)
      assert_eq(v[0], "a")
      assert_eq(v[1], "b")
      assert_eq(v[2], "c")
      assert_eq(v[3], "d")
      assert_eq(v[4], "e")
    }
    _ => assert_true(false)
  }
  
  // Test int array
  let int_array = ArrayIntValue([1, 2, 3, 4, 5])
  let serialized_int_array = AttributeSerializer::serialize(int_array)
  let deserialized_int_array = AttributeSerializer::deserialize(serialized_int_array)
  
  match deserialized_int_array {
    ArrayIntValue(v) => {
      assert_eq(v.length(), 5)
      assert_eq(v[0], 1)
      assert_eq(v[1], 2)
      assert_eq(v[2], 3)
      assert_eq(v[3], 4)
      assert_eq(v[4], 5)
    }
    _ => assert_true(false)
  }
  
  // Test empty array
  let empty_array = ArrayStringValue([])
  let serialized_empty = AttributeSerializer::serialize(empty_array)
  let deserialized_empty = AttributeSerializer::deserialize(serialized_empty)
  
  match deserialized_empty {
    ArrayStringValue(v) => assert_eq(v.length(), 0)
    _ => assert_true(false)
  }
}

test "span context serialization and deserialization" {
  let span_ctx = SpanContext::new("trace_123456789", "span_987654321", true, "test_state")
  
  // Serialize span context
  let serialized_ctx = SpanContextSerializer::serialize(span_ctx)
  assert_true(serialized_ctx.length() > 0)
  
  // Deserialize span context
  let deserialized_ctx = SpanContextSerializer::deserialize(serialized_ctx)
  
  // Verify deserialized context matches original
  assert_eq(SpanContext::trace_id(deserialized_ctx), "trace_123456789")
  assert_eq(SpanContext::span_id(deserialized_ctx), "span_987654321")
  assert_true(SpanContext::is_sampled(deserialized_ctx))
  assert_eq(SpanContext::state(deserialized_ctx), "test_state")
  assert_true(SpanContext::is_valid(deserialized_ctx))
  
  // Test invalid context serialization
  let invalid_ctx = SpanContext::new("", "", false, "")
  let serialized_invalid = SpanContextSerializer::serialize(invalid_ctx)
  let deserialized_invalid = SpanContextSerializer::deserialize(serialized_invalid)
  
  assert_false(SpanContext::is_valid(deserialized_invalid))
  assert_false(SpanContext::is_sampled(deserialized_invalid))
}

test "span serialization and deserialization" {
  let span_ctx = SpanContext::new("trace_span_test", "span_test_id", true, "")
  let span = Span::new("test_span", Client, span_ctx)
  
  // Add some data to the span
  let attrs = Attributes::new()
  Attributes::set(attrs, "test.key", StringValue("test.value"))
  Attributes::set(attrs, "test.number", IntValue(42))
  
  Span::add_event(span, "test_event", Some(attrs))
  Span::set_status(span, Ok, Some("Test completed"))
  
  // Serialize span
  let serialized_span = SpanSerializer::serialize(span)
  assert_true(serialized_span.length() > 0)
  
  // Deserialize span
  let deserialized_span = SpanSerializer::deserialize(serialized_span)
  
  // Verify deserialized span matches original
  assert_eq(Span::name(deserialized_span), "test_span")
  match Span::kind(deserialized_span) {
    Client => assert_true(true)
    _ => assert_true(false)
  }
  
  let deserialized_ctx = Span::span_context(deserialized_span)
  assert_eq(SpanContext::trace_id(deserialized_ctx), "trace_span_test")
  assert_eq(SpanContext::span_id(deserialized_ctx), "span_test_id")
  assert_true(SpanContext::is_sampled(deserialized_ctx))
  
  assert_eq(Span::status(deserialized_span), Ok)
}

test "log record serialization and deserialization" {
  let log_record = LogRecord::new(
    Error,
    Some("Test error message"),
    Some(Attributes::new()),
    Some(1234567890L),
    Some(1234567891L),
    Some("log_trace_id"),
    Some("log_span_id")
  )
  
  // Serialize log record
  let serialized_log = LogRecordSerializer::serialize(log_record)
  assert_true(serialized_log.length() > 0)
  
  // Deserialize log record
  let deserialized_log = LogRecordSerializer::deserialize(serialized_log)
  
  // Verify deserialized log matches original
  assert_eq(LogRecord::severity_number(deserialized_log), Error)
  match LogRecord::body(deserialized_log) {
    Some(body) => assert_eq(body, "Test error message")
    None => assert_true(false)
  }
  
  assert_eq(LogRecord::trace_id(deserialized_log), Some("log_trace_id"))
  assert_eq(LogRecord::span_id(deserialized_log), Some("log_span_id"))
}

test "metrics serialization and deserialization" {
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "serialization_meter")
  
  // Create and record metrics
  let counter = Meter::create_counter(meter, "test_counter", Some("Test counter"), Some("count"))
  Counter::add(counter, 10.5)
  
  let histogram = Meter::create_histogram(meter, "test_histogram", Some("Test histogram"), Some("ms"))
  Histogram::record(histogram, 100.0)
  Histogram::record(histogram, 200.0)
  Histogram::record(histogram, 300.0)
  
  // Serialize metrics
  let serialized_metrics = MetricsSerializer::serialize(provider)
  assert_true(serialized_metrics.length() > 0)
  
  // Deserialize metrics
  let deserialized_provider = MetricsSerializer::deserialize(serialized_metrics)
  
  // Verify deserialized metrics
  let deserialized_meter = MeterProvider::get_meter(deserialized_provider, "serialization_meter")
  assert_true(Meter::is_valid(deserialized_meter))
  
  // Note: In a real implementation, we would verify the actual metric values
  // This is a simplified test that focuses on the serialization/deserialization process
}

test "resource serialization and deserialization" {
  let resource = Resource::new()
  let attrs = [
    ("service.name", StringValue("test_service")),
    ("service.version", StringValue("1.0.0")),
    ("service.instance.id", StringValue("instance-123")),
    ("environment", StringValue("test")),
    ("region", StringValue("us-west-2"))
  ]
  let resource_with_attrs = Resource::with_attributes(resource, attrs)
  
  // Serialize resource
  let serialized_resource = ResourceSerializer::serialize(resource_with_attrs)
  assert_true(serialized_resource.length() > 0)
  
  // Deserialize resource
  let deserialized_resource = ResourceSerializer::deserialize(serialized_resource)
  
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
  
  let environment = Resource::get_attribute(deserialized_resource, "environment")
  match environment {
    Some(StringValue(env)) => assert_eq(env, "test")
    _ => assert_true(false)
  }
}

test "baggage serialization and deserialization" {
  let baggage = Baggage::new()
  
  // Add entries to baggage
  let updated_baggage = Baggage::set_entry(baggage, "key1", "value1")
  updated_baggage = Baggage::set_entry(updated_baggage, "key2", "value2")
  updated_baggage = Baggage::set_entry(updated_baggage, "key3", "value3")
  
  // Serialize baggage
  let serialized_baggage = BaggageSerializer::serialize(updated_baggage)
  assert_true(serialized_baggage.length() > 0)
  
  // Deserialize baggage
  let deserialized_baggage = BaggageSerializer::deserialize(serialized_baggage)
  
  // Verify deserialized baggage entries
  let value1 = Baggage::get_entry(deserialized_baggage, "key1")
  match value1 {
    Some(v) => assert_eq(v, "value1")
    None => assert_true(false)
  }
  
  let value2 = Baggage::get_entry(deserialized_baggage, "key2")
  match value2 {
    Some(v) => assert_eq(v, "value2")
    None => assert_true(false)
  }
  
  let value3 = Baggage::get_entry(deserialized_baggage, "key3")
  match value3 {
    Some(v) => assert_eq(v, "value3")
    None => assert_true(false)
  }
  
  // Verify non-existent entry
  let non_existent = Baggage::get_entry(deserialized_baggage, "non_existent_key")
  match non_existent {
    Some(_) => assert_true(false)
    None => assert_true(true)
  }
}

test "complex nested attributes serialization" {
  // Create complex nested attributes
  let outer_attrs = Attributes::new()
  Attributes::set(outer_attrs, "outer.key1", StringValue("outer_value1"))
  Attributes::set(outer_attrs, "outer.key2", IntValue(42))
  
  // Create inner attributes
  let inner_attrs = Attributes::new()
  Attributes::set(inner_attrs, "inner.key1", StringValue("inner_value1"))
  Attributes::set(inner_attrs, "inner.key2", FloatValue(3.14))
  
  // Nest inner attributes in outer
  Attributes::set(outer_attrs, "nested", AttributesValue(inner_attrs))
  
  // Serialize complex attributes
  let serialized_attrs = AttributesSerializer::serialize(outer_attrs)
  assert_true(serialized_attrs.length() > 0)
  
  // Deserialize complex attributes
  let deserialized_attrs = AttributesSerializer::deserialize(serialized_attrs)
  
  // Verify deserialized attributes
  let outer_value1 = Attributes::get(deserialized_attrs, "outer.key1")
  match outer_value1 {
    StringValue(v) => assert_eq(v, "outer_value1")
    _ => assert_true(false)
  }
  
  let outer_value2 = Attributes::get(deserialized_attrs, "outer.key2")
  match outer_value2 {
    IntValue(v) => assert_eq(v, 42)
    _ => assert_true(false)
  }
  
  // Verify nested attributes
  let nested_attrs_value = Attributes::get(deserialized_attrs, "nested")
  match nested_attrs_value {
    AttributesValue(nested_attrs) => {
      let inner_value1 = Attributes::get(nested_attrs, "inner.key1")
      match inner_value1 {
        StringValue(v) => assert_eq(v, "inner_value1")
        _ => assert_true(false)
      }
      
      let inner_value2 = Attributes::get(nested_attrs, "inner.key2")
      match inner_value2 {
        FloatValue(v) => assert_true(abs(v - 3.14) < 0.00001)
        _ => assert_true(false)
      }
    }
    _ => assert_true(false)
  }
}

test "batch serialization and deserialization" {
  // Create multiple telemetry objects for batch serialization
  let spans = []
  let log_records = []
  
  // Create spans
  for i in 0..5 {
    let span_ctx = SpanContext::new("batch_trace_" + i.to_string(), "batch_span_" + i.to_string(), true, "")
    let span = Span::new("batch_span_" + i.to_string(), Server, span_ctx)
    spans.push(span)
  }
  
  // Create log records
  for i in 0..5 {
    let log_record = LogRecord::new(
      Info,
      Some("Batch log message " + i.to_string()),
      Some(Attributes::new()),
      Some(1234567890L + i),
      Some(1234567891L + i),
      Some("batch_trace_" + i.to_string()),
      Some("batch_span_" + i.to_string())
    )
    log_records.push(log_record)
  }
  
  // Serialize batch
  let serialized_batch = BatchSerializer::serialize(spans, log_records)
  assert_true(serialized_batch.length() > 0)
  
  // Deserialize batch
  let (deserialized_spans, deserialized_logs) = BatchSerializer::deserialize(serialized_batch)
  
  // Verify deserialized spans
  assert_eq(deserialized_spans.length(), 5)
  for i in 0..5 {
    assert_eq(Span::name(deserialized_spans[i]), "batch_span_" + i.to_string())
    match Span::kind(deserialized_spans[i]) {
      Server => assert_true(true)
      _ => assert_true(false)
    }
    
    let ctx = Span::span_context(deserialized_spans[i])
    assert_eq(SpanContext::trace_id(ctx), "batch_trace_" + i.to_string())
    assert_eq(SpanContext::span_id(ctx), "batch_span_" + i.to_string())
  }
  
  // Verify deserialized log records
  assert_eq(deserialized_logs.length(), 5)
  for i in 0..5 {
    assert_eq(LogRecord::severity_number(deserialized_logs[i]), Info)
    match LogRecord::body(deserialized_logs[i]) {
      Some(body) => assert_eq(body, "Batch log message " + i.to_string())
      None => assert_true(false)
    }
    
    assert_eq(LogRecord::trace_id(deserialized_logs[i]), Some("batch_trace_" + i.to_string()))
    assert_eq(LogRecord::span_id(deserialized_logs[i]), Some("batch_span_" + i.to_string()))
  }
}

test "serialization format compatibility" {
  // Test compatibility with different serialization formats
  
  // Create test data
  let span_ctx = SpanContext::new("compat_trace", "compat_span", true, "")
  let span = Span::new("compatibility_test", Internal, span_ctx)
  
  // Test JSON format
  let json_serializer = JsonSerializer::new()
  let json_serialized = json_serializer.serialize_span(span)
  let json_deserialized = json_serializer.deserialize_span(json_serialized)
  
  assert_eq(Span::name(json_deserialized), "compatibility_test")
  assert_eq(SpanContext::trace_id(Span::span_context(json_deserialized)), "compat_trace")
  assert_eq(SpanContext::span_id(Span::span_context(json_deserialized)), "compat_span")
  
  // Test binary format
  let binary_serializer = BinarySerializer::new()
  let binary_serialized = binary_serializer.serialize_span(span)
  let binary_deserialized = binary_serializer.deserialize_span(binary_serialized)
  
  assert_eq(Span::name(binary_deserialized), "compatibility_test")
  assert_eq(SpanContext::trace_id(Span::span_context(binary_deserialized)), "compat_trace")
  assert_eq(SpanContext::span_id(Span::span_context(binary_deserialized)), "compat_span")
  
  // Verify both formats produce equivalent results
  assert_eq(Span::name(json_deserialized), Span::name(binary_deserialized))
  assert_eq(SpanContext::trace_id(Span::span_context(json_deserialized)), 
            SpanContext::trace_id(Span::span_context(binary_deserialized)))
  assert_eq(SpanContext::span_id(Span::span_context(json_deserialized)), 
            SpanContext::span_id(Span::span_context(binary_deserialized)))
}