// Azimuth Telemetry System - Serialization/Deserialization Tests
// This file contains test cases for telemetry data serialization and deserialization

// Test 1: Span Serialization to JSON
test "span serialization to json" {
  let trace_id = "0af7651916cd43dd8448eb211c80319c"
  let span_id = "b7ad6b7169203331"
  let span_ctx = SpanContext::new(trace_id, span_id, true, "")
  let span = Span::new("test_span", Internal, span_ctx)
  
  // Add some attributes and events
  let attrs = Attributes::new()
  Attributes::set(attrs, "http.method", StringValue("GET"))
  Attributes::set(attrs, "http.url", StringValue("https://example.com/api"))
  Span::set_attributes(span, attrs)
  
  Span::add_event(span, "start_event", Some([("event.type", StringValue("system"))]))
  Span::set_status(span, Ok, Some("Operation completed"))
  
  // Serialize span to JSON
  let json_str = SpanSerializer::to_json(span)
  
  // Verify JSON contains expected fields
  assert_true(json_str.contains("trace_id"))
  assert_true(json_str.contains("span_id"))
  assert_true(json_str.contains("test_span"))
  assert_true(json_str.contains("attributes"))
  assert_true(json_str.contains("events"))
  assert_true(json_str.contains("status"))
  assert_true(json_str.contains("http.method"))
  assert_true(json_str.contains("GET"))
}

// Test 2: Span Deserialization from JSON
test "span deserialization from json" {
  let json_str = "{"
    + "\"trace_id\":\"0af7651916cd43dd8448eb211c80319c\","
    + "\"span_id\":\"b7ad6b7169203331\","
    + "\"name\":\"test_span\","
    + "\"kind\":\"INTERNAL\","
    + "\"status\":{\"code\":\"OK\",\"message\":\"Operation completed\"},"
    + "\"attributes\":{\"http.method\":\"GET\",\"http.url\":\"https://example.com/api\"},"
    + "\"events\":[{\"name\":\"start_event\",\"attributes\":{\"event.type\":\"system\"}}],"
    + "\"start_time\":1234567890,"
    + "\"end_time\":1234567895"
    + "}"
  
  // Deserialize span from JSON
  let span = SpanDeserializer::from_json(json_str)
  
  // Verify deserialized span
  assert_eq(Span::name(span), "test_span")
  match Span::kind(span) {
    Internal => assert_true(true)
    _ => assert_true(false)
  }
  
  let retrieved_ctx = Span::span_context(span)
  assert_eq(SpanContext::trace_id(retrieved_ctx), "0af7651916cd43dd8448eb211c80319c")
  assert_eq(SpanContext::span_id(retrieved_ctx), "b7ad6b7169203331")
}

// Test 3: Metric Serialization to Binary
test "metric serialization to binary" {
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "test_meter")
  let counter = Meter::create_counter(meter, "test_counter", Some("Test counter"), Some("count"))
  
  // Add some measurements
  Counter::add(counter, 1.0)
  Counter::add(counter, 2.5, Some(Attributes::new()))
  
  // Serialize metric to binary
  let binary_data = MetricSerializer::to_binary(counter)
  
  // Verify binary data is not empty
  assert_true(binary_data.length() > 0)
  
  // Deserialize metric from binary
  let deserialized_counter = MetricDeserializer::from_binary(binary_data)
  
  // Verify deserialized metric
  let instrument = Counter::as_instrument(deserialized_counter)
  assert_eq(Instrument::name(instrument), "test_counter")
  assert_eq(Instrument::description(instrument), Some("Test counter"))
  assert_eq(Instrument::unit(instrument), Some("count"))
}

// Test 4: LogRecord Serialization to Protobuf
test "log record serialization to protobuf" {
  let attrs = Attributes::new()
  Attributes::set(attrs, "log.level", StringValue("INFO"))
  Attributes::set(attrs, "service.name", StringValue("test_service"))
  
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
  
  // Serialize log record to protobuf
  let protobuf_data = LogRecordSerializer::to_protobuf(log_record)
  
  // Verify protobuf data is not empty
  assert_true(protobuf_data.length() > 0)
  
  // Deserialize log record from protobuf
  let deserialized_log = LogRecordDeserializer::from_protobuf(protobuf_data)
  
  // Verify deserialized log record
  assert_eq(LogRecord::severity_number(deserialized_log), Info)
  match LogRecord::body(deserialized_log) {
    Some(body) => assert_eq(body, "Test log message")
    None => assert_true(false)
  }
  assert_eq(LogRecord::trace_id(deserialized_log), Some("trace_id"))
  assert_eq(LogRecord::span_id(deserialized_log), Some("span_id"))
}

// Test 5: Resource Serialization to YAML
test "resource serialization to yaml" {
  let resource = Resource::new()
  let attrs = [
    ("service.name", StringValue("test_service")),
    ("service.version", StringValue("1.0.0")),
    ("service.instance.id", StringValue("instance-123")),
    ("deployment.environment", StringValue("production"))
  ]
  let resource_with_attrs = Resource::with_attributes(resource, attrs)
  
  // Serialize resource to YAML
  let yaml_str = ResourceSerializer::to_yaml(resource_with_attrs)
  
  // Verify YAML contains expected fields
  assert_true(yaml_str.contains("service.name"))
  assert_true(yaml_str.contains("test_service"))
  assert_true(yaml_str.contains("service.version"))
  assert_true(yaml_str.contains("1.0.0"))
  assert_true(yaml_str.contains("deployment.environment"))
  assert_true(yaml_str.contains("production"))
}

// Test 6: Resource Deserialization from YAML
test "resource deserialization from yaml" {
  let yaml_str = "attributes:\n"
    + "  service.name: test_service\n"
    + "  service.version: \"1.0.0\"\n"
    + "  service.instance.id: instance-123\n"
    + "  deployment.environment: production\n"
  
  // Deserialize resource from YAML
  let resource = ResourceDeserializer::from_yaml(yaml_str)
  
  // Verify deserialized resource
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
  
  let environment = Resource::get_attribute(resource, "deployment.environment")
  match environment {
    Some(StringValue(env)) => assert_eq(env, "production")
    _ => assert_true(false)
  }
}

// Test 7: Batch Serialization
test "batch serialization" {
  // Create multiple spans
  let spans = []
  
  for i in 0..=5 {
    let trace_id = "trace_" + i.to_string()
    let span_id = "span_" + i.to_string()
    let span_ctx = SpanContext::new(trace_id, span_id, true, "")
    let span = Span::new("span_" + i.to_string(), Internal, span_ctx)
    
    let attrs = Attributes::new()
    Attributes::set(attrs, "batch.index", IntValue(i))
    Span::set_attributes(span, attrs)
    
    spans.push(span)
  }
  
  // Serialize spans to JSON array
  let json_array = SpanSerializer::batch_to_json(spans)
  
  // Verify JSON array contains all spans
  assert_true(json_array.contains("["))
  assert_true(json_array.contains("]"))
  assert_true(json_array.contains("trace_0"))
  assert_true(json_array.contains("trace_5"))
  assert_true(json_array.contains("batch.index"))
  
  // Deserialize spans from JSON array
  let deserialized_spans = SpanDeserializer::batch_from_json(json_array)
  
  // Verify all spans were deserialized
  assert_eq(deserialized_spans.length(), spans.length())
  
  for i in 0..=5 {
    let span = deserialized_spans[i]
    assert_eq(Span::name(span), "span_" + i.to_string())
    
    let attrs = Span::attributes(span)
    let batch_index = Attributes::get(attrs, "batch.index")
    match batch_index {
      Some(IntValue(index)) => assert_eq(index, i)
      _ => assert_true(false)
    }
  }
}

// Test 8: Compression Serialization
test "compression serialization" {
  let trace_id = "0af7651916cd43dd8448eb211c80319c"
  let span_id = "b7ad6b7169203331"
  let span_ctx = SpanContext::new(trace_id, span_id, true, "")
  let span = Span::new("large_test_span", Internal, span_ctx)
  
  // Add many attributes to create a large span
  let attrs = Attributes::new()
  for i in 0..=100 {
    Attributes::set(attrs, "attr_" + i.to_string(), StringValue("value_" + i.to_string()))
  }
  Span::set_attributes(span, attrs)
  
  // Add many events
  for i in 0..=50 {
    Span::add_event(span, "event_" + i.to_string(), Some([("event.index", IntValue(i))]))
  }
  
  // Serialize span to JSON
  let json_str = SpanSerializer::to_json(span)
  let original_size = json_str.length()
  
  // Compress the JSON
  let compressed_data = CompressionUtil::compress(json_str)
  let compressed_size = compressed_data.length()
  
  // Verify compression reduces size
  assert_true(compressed_size < original_size)
  
  // Decompress the JSON
  let decompressed_json = CompressionUtil::decompress(compressed_data)
  
  // Verify decompressed JSON matches original
  assert_eq(decompressed_json, json_str)
  
  // Deserialize span from decompressed JSON
  let deserialized_span = SpanDeserializer::from_json(decompressed_json)
  
  // Verify deserialized span
  assert_eq(Span::name(deserialized_span), "large_test_span")
  
  let retrieved_attrs = Span::attributes(deserialized_span)
  let attr_0 = Attributes::get(retrieved_attrs, "attr_0")
  match attr_0 {
    Some(StringValue(value)) => assert_eq(value, "value_0")
    _ => assert_true(false)
  }
  
  let attr_100 = Attributes::get(retrieved_attrs, "attr_100")
  match attr_100 {
    Some(StringValue(value)) => assert_eq(value, "value_100")
    _ => assert_true(false)
  }
}

// Test 9: Cross-Format Serialization
test "cross-format serialization" {
  let trace_id = "0af7651916cd43dd8448eb211c80319c"
  let span_id = "b7ad6b7169203331"
  let span_ctx = SpanContext::new(trace_id, span_id, true, "")
  let span = Span::new("cross_format_test", Internal, span_ctx)
  
  let attrs = Attributes::new()
  Attributes::set(attrs, "test.attr", StringValue("test_value"))
  Span::set_attributes(span, attrs)
  
  // Serialize to JSON
  let json_str = SpanSerializer::to_json(span)
  
  // Convert JSON to YAML
  let yaml_str = FormatConverter::json_to_yaml(json_str)
  
  // Convert YAML to binary
  let binary_data = FormatConverter::yaml_to_binary(yaml_str)
  
  // Convert binary back to JSON
  let final_json = FormatConverter::binary_to_json(binary_data)
  
  // Deserialize span from final JSON
  let final_span = SpanDeserializer::from_json(final_json)
  
  // Verify final span matches original
  assert_eq(Span::name(final_span), "cross_format_test")
  
  let final_attrs = Span::attributes(final_span)
  let test_attr = Attributes::get(final_attrs, "test.attr")
  match test_attr {
    Some(StringValue(value)) => assert_eq(value, "test_value")
    _ => assert_true(false)
  }
}

// Test 10: Error Handling in Serialization
test "error handling in serialization" {
  // Test with invalid JSON
  let invalid_json = "{ invalid json }"
  
  let result = SpanDeserializer::from_json(invalid_json)
  match result {
    Error(_) => assert_true(true)
    _ => assert_true(false)
  }
  
  // Test with empty binary data
  let empty_binary = []
  let result2 = MetricDeserializer::from_binary(empty_binary)
  match result2 {
    Error(_) => assert_true(true)
    _ => assert_true(false)
  }
  
  // Test with malformed YAML
  let malformed_yaml = "invalid: yaml: content:\n  - missing\n    proper"
  
  let result3 = ResourceDeserializer::from_yaml(malformed_yaml)
  match result3 {
    Error(_) => assert_true(true)
    _ => assert_true(false)
  }
  
  // Test with corrupted protobuf data
  let corrupted_protobuf = [0x80, 0x81, 0x82, 0x83] // Invalid protobuf
  
  let result4 = LogRecordDeserializer::from_protobuf(corrupted_protobuf)
  match result4 {
    Error(_) => assert_true(true)
    _ => assert_true(false)
  }
}