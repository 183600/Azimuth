// Comprehensive Serialization/Deserialization Tests for Azimuth Telemetry System
// This file contains tests for data serialization and deserialization functionality

test "telemetry data serialization to JSON format" {
  // Test basic span serialization
  let span_ctx = SpanContext::new("trace123", "span456", true, "")
  let span = Span::new("test-span", Internal, span_ctx)
  Span::add_event(span, "test-event", Some([("key1", StringValue("value1"))]))
  
  // Serialize span to JSON
  let serialized_span = SpanSerializer::to_json(span)
  
  // Verify serialized JSON contains expected fields
  assert_true(serialized_span.contains("\"name\":\"test-span\""))
  assert_true(serialized_span.contains("\"traceId\":\"trace123\""))
  assert_true(serialized_span.contains("\"spanId\":\"span456\""))
  assert_true(serialized_span.contains("\"events\":[{\"name\":\"test-event\""))
  assert_true(serialized_span.contains("\"attributes\":{\"key1\":\"value1\"}"))
}

test "telemetry data deserialization from JSON format" {
  // JSON string representing a span
  let json_span = "{\"name\":\"deserialized-span\",\"traceId\":\"trace789\",\"spanId\":\"span012\",\"kind\":\"INTERNAL\",\"status\":{\"code\":\"OK\"},\"attributes\":{\"key2\":\"value2\",\"key3\":42},\"events\":[{\"name\":\"deserialized-event\",\"timestamp\":1234567890}]}"
  
  // Deserialize JSON to span
  let deserialized_span = SpanDeserializer::from_json(json_span)
  
  // Verify deserialized span properties
  assert_eq(Span::name(deserialized_span), "deserialized-span")
  assert_eq(SpanContext::trace_id(Span::span_context(deserialized_span)), "trace789")
  assert_eq(SpanContext::span_id(Span::span_context(deserialized_span)), "span012")
  assert_eq(Span::kind(deserialized_span), Internal)
  assert_eq(Span::status(deserialized_span), Ok)
  
  // Verify attributes
  let attr_value1 = Span::get_attribute(deserialized_span, "key2")
  match attr_value1 {
    Some(StringValue(v)) => assert_eq(v, "value2")
    _ => assert_true(false)
  }
  
  let attr_value2 = Span::get_attribute(deserialized_span, "key3")
  match attr_value2 {
    Some(IntValue(v)) => assert_eq(v, 42)
    _ => assert_true(false)
  }
}

test "metrics serialization to binary format" {
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "serialization-test")
  let counter = Meter::create_counter(meter, "test-counter", Some("Test counter"), Some("count"))
  
  // Add multiple measurements
  Counter::add(counter, 1.0)
  Counter::add(counter, 2.5, Some(Attributes::with([("tag1", StringValue("value1"))])))
  Counter::add(counter, 3.7, Some(Attributes::with([("tag2", StringValue("value2"))])))
  
  // Serialize metrics to binary format
  let serialized_metrics = MetricsSerializer::to_binary(counter)
  
  // Verify binary data is not empty
  assert_true(serialized_metrics.length() > 0)
  
  // Verify binary data contains expected signature
  assert_eq(serialized_metrics[0], 0x4D)  // 'M' signature
  assert_eq(serialized_metrics[1], 0x54)  // 'T' signature
}

test "metrics deserialization from binary format" {
  // Create binary data representing metrics
  let binary_data = [0x4D, 0x54, 0x01, 0x00, 0x0C, 0x00, 0x74, 0x65, 0x73, 0x74, 0x2D, 0x63, 0x6F, 0x75, 0x6E, 0x74, 0x65, 0x72]
  
  // Deserialize binary to metrics
  let deserialized_metrics = MetricsDeserializer::from_binary(binary_data)
  
  // Verify deserialized metrics properties
  assert_eq(Metrics::name(deserialized_metrics), "test-counter")
  assert_eq(Metrics::description(deserialized_metrics), Some("Test counter"))
  assert_eq(Metrics::unit(deserialized_metrics), Some("count"))
  assert_eq(Metrics::type_(deserialized_metrics), Counter)
}

test "log record serialization to protocol buffers" {
  let log_record = LogRecord::new_with_context(
    Error,
    Some("Error processing request"),
    Some(Attributes::with([
      ("error.code", IntValue(500)),
      ("error.message", StringValue("Internal Server Error")),
      ("request.id", StringValue("req-123"))
    ])),
    Some(1234567890L),
    Some(1234567891L),
    Some("trace123"),
    Some("span456"),
    Some(Context::root())
  )
  
  // Serialize log record to protocol buffers
  let serialized_log = LogSerializer::to_protobuf(log_record)
  
  // Verify protobuf data is not empty
  assert_true(serialized_log.length() > 0)
  
  // Verify protobuf contains expected field markers
  assert_true(serialized_log.contains_byte(0x0A))  // Field 1 (severity)
  assert_true(serialized_log.contains_byte(0x12))  // Field 2 (body)
  assert_true(serialized_log.contains_byte(0x1A))  // Field 3 (attributes)
}

test "log record deserialization from protocol buffers" {
  // Create protobuf data representing a log record
  let protobuf_data = LogRecordBuilder::new()
    .with_severity(Error)
    .with_body("Deserialized error message")
    .with_attribute("error.code", IntValue(404))
    .with_attribute("error.type", StringValue("Not Found"))
    .with_timestamp(1234567890L)
    .build()
  
  // Deserialize protobuf to log record
  let deserialized_log = LogDeserializer::from_protobuf(protobuf_data)
  
  // Verify deserialized log properties
  assert_eq(LogRecord::severity_number(deserialized_log), Error)
  match LogRecord::body(deserialized_log) {
    Some(body) => assert_eq(body, "Deserialized error message")
    None => assert_true(false)
  }
  assert_eq(LogRecord::timestamp(deserialized_log), Some(1234567890L))
  
  // Verify attributes
  let error_code = LogRecord::get_attribute(deserialized_log, "error.code")
  match error_code {
    Some(IntValue(v)) => assert_eq(v, 404)
    _ => assert_true(false)
  }
  
  let error_type = LogRecord::get_attribute(deserialized_log, "error.type")
  match error_type {
    Some(StringValue(v)) => assert_eq(v, "Not Found")
    _ => assert_true(false)
  }
}

test "resource serialization to YAML format" {
  let resource = Resource::with_attributes(Resource::new(), [
    ("service.name", StringValue("test-service")),
    ("service.version", StringValue("1.2.3")),
    ("service.instance.id", StringValue("instance-456")),
    ("deployment.environment", StringValue("production")),
    ("host.name", StringValue("host-123"))
  ])
  
  // Serialize resource to YAML
  let yaml_resource = ResourceSerializer::to_yaml(resource)
  
  // Verify YAML contains expected fields
  assert_true(yaml_resource.contains("service.name: test-service"))
  assert_true(yaml_resource.contains("service.version: \"1.2.3\""))
  assert_true(yaml_resource.contains("service.instance.id: instance-456"))
  assert_true(yaml_resource.contains("deployment.environment: production"))
  assert_true(yaml_resource.contains("host.name: host-123"))
}

test "resource deserialization from YAML format" {
  // YAML string representing a resource
  let yaml_resource = "service.name: yaml-test-service\nservice.version: \"2.0.0\"\nservice.instance.id: yaml-instance-789\ndeployment.environment: staging\nhost.name: yaml-host-456\n"
  
  // Deserialize YAML to resource
  let deserialized_resource = ResourceDeserializer::from_yaml(yaml_resource)
  
  // Verify deserialized resource properties
  let service_name = Resource::get_attribute(deserialized_resource, "service.name")
  match service_name {
    Some(StringValue(v)) => assert_eq(v, "yaml-test-service")
    _ => assert_true(false)
  }
  
  let service_version = Resource::get_attribute(deserialized_resource, "service.version")
  match service_version {
    Some(StringValue(v)) => assert_eq(v, "2.0.0")
    _ => assert_true(false)
  }
  
  let instance_id = Resource::get_attribute(deserialized_resource, "service.instance.id")
  match instance_id {
    Some(StringValue(v)) => assert_eq(v, "yaml-instance-789")
    _ => assert_true(false)
  }
  
  let environment = Resource::get_attribute(deserialized_resource, "deployment.environment")
  match environment {
    Some(StringValue(v)) => assert_eq(v, "staging")
    _ => assert_true(false)
  }
}

test "context propagation serialization to custom format" {
  let ctx = Context::root()
  let key1 = ContextKey::new("custom.key1")
  let key2 = ContextKey::new("custom.key2")
  let key3 = ContextKey::new("custom.key3")
  
  let ctx_with_values = Context::with_value(
    Context::with_value(
      Context::with_value(ctx, key1, "value1"),
      key2, "value2"
    ),
    key3, "value3"
  )
  
  // Serialize context to custom format
  let serialized_ctx = ContextSerializer::to_custom(ctx_with_values)
  
  // Verify custom format contains expected key-value pairs
  assert_true(serialized_ctx.contains("custom.key1=value1"))
  assert_true(serialized_ctx.contains("custom.key2=value2"))
  assert_true(serialized_ctx.contains("custom.key3=value3"))
}

test "context propagation deserialization from custom format" {
  // Custom format string representing context
  let custom_ctx = "custom.key1=deserialized-value1,custom.key2=deserialized-value2,custom.key3=deserialized-value3"
  
  // Deserialize custom format to context
  let deserialized_ctx = ContextDeserializer::from_custom(custom_ctx)
  
  // Verify deserialized context properties
  let key1 = ContextKey::new("custom.key1")
  let key2 = ContextKey::new("custom.key2")
  let key3 = ContextKey::new("custom.key3")
  
  let value1 = Context::get(deserialized_ctx, key1)
  let value2 = Context::get(deserialized_ctx, key2)
  let value3 = Context::get(deserialized_ctx, key3)
  
  assert_eq(value1, Some("deserialized-value1"))
  assert_eq(value2, Some("deserialized-value2"))
  assert_eq(value3, Some("deserialized-value3"))
}

test "serialization error handling and edge cases" {
  // Test serialization of null/empty values
  let empty_span = Span::new("", Internal, SpanContext::new("", "", false, ""))
  let serialized_empty = SpanSerializer::to_json(empty_span)
  assert_true(serialized_empty.contains("\"name\":\"\""))
  assert_true(serialized_empty.contains("\"traceId\":\"\""))
  
  // Test deserialization of invalid JSON
  let invalid_json = "{\"invalid\": json}"
  let deserialized_invalid = SpanDeserializer::from_json(invalid_json)
  assert_eq(Span::name(deserialized_invalid), "fallback-name")
  
  // Test serialization of special characters
  let special_chars_span = Span::new("span-with-\"quotes\" and \n newlines", Internal, SpanContext::new("trace", "span", true, ""))
  Span::add_event(special_chars_span, "event-with-\"quotes\"", Some([("key", StringValue("value-with-\"quotes\""))]))
  let serialized_special = SpanSerializer::to_json(special_chars_span)
  assert_true(serialized_special.contains("\\\""))
  assert_true(serialized_special.contains("\\n"))
  
  // Test deserialization of missing fields
  let incomplete_json = "{\"name\":\"incomplete-span\"}"
  let deserialized_incomplete = SpanDeserializer::from_json(incomplete_json)
  assert_eq(Span::name(deserialized_incomplete), "incomplete-span")
  assert_eq(SpanContext::trace_id(Span::span_context(deserialized_incomplete)), "")
}