// Azimuth Telemetry System - Enhanced Serialization Format Tests
// This file contains test cases for data serialization and deserialization

// Test 1: JSON Serialization and Deserialization
test "json serialization and deserialization of telemetry data" {
  // Create a telemetry span with various attributes
  let span_ctx = SpanContext::new("0af7651916cd43dd8448eb211c80319c", "b7ad6b7169203331", true, "test_state")
  let span = Span::new("test_span", Internal, span_ctx)
  
  // Add various types of attributes
  Span::set_attribute(span, "string_attr", StringValue("test_value"))
  Span::set_attribute(span, "int_attr", IntValue(42))
  Span::set_attribute(span, "float_attr", FloatValue(3.14))
  Span::set_attribute(span, "bool_attr", BoolValue(true))
  Span::set_attribute(span, "array_attr", ArrayStringValue(["a", "b", "c"]))
  
  // Add events
  Span::add_event(span, "test_event", Some([("event_attr", StringValue("event_value"))]))
  
  // Serialize to JSON
  let json_data = JsonSerializer::serialize_span(span)
  assert_true(json_data.length() > 0)
  
  // Deserialize from JSON
  let deserialized_span = JsonSerializer::deserialize_span(json_data)
  
  // Verify deserialized data matches original
  assert_eq(Span::name(deserialized_span), Span::name(span))
  assert_eq(Span::kind(deserialized_span), Span::kind(span))
  
  let original_ctx = Span::span_context(span)
  let deserialized_ctx = Span::span_context(deserialized_span)
  assert_eq(SpanContext::trace_id(original_ctx), SpanContext::trace_id(deserialized_ctx))
  assert_eq(SpanContext::span_id(original_ctx), SpanContext::span_id(deserialized_ctx))
}

// Test 2: Binary Serialization and Deserialization
test "binary serialization and deserialization of telemetry data" {
  // Create a telemetry metrics batch
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "test_meter")
  
  let counter = Meter::create_counter(meter, "test_counter", Some("Test counter"), Some("count"))
  Counter::add(counter, 1.0)
  Counter::add(counter, 2.5, Some(Attributes::new()))
  
  let histogram = Meter::create_histogram(meter, "test_histogram", Some("Test histogram"), Some("ms"))
  Histogram::record(histogram, 100.0)
  Histogram::record(histogram, 200.5, Some(Attributes::new()))
  
  // Create metrics batch
  let metrics_batch = MetricsBatch::new()
  MetricsBatch::add_counter(metrics_batch, counter)
  MetricsBatch::add_histogram(metrics_batch, histogram)
  
  // Serialize to binary format
  let binary_data = BinarySerializer::serialize_metrics_batch(metrics_batch)
  assert_true(binary_data.length() > 0)
  
  // Deserialize from binary format
  let deserialized_batch = BinarySerializer::deserialize_metrics_batch(binary_data)
  
  // Verify deserialized data matches original
  let original_counters = MetricsBatch::get_counters(metrics_batch)
  let deserialized_counters = MetricsBatch::get_counters(deserialized_batch)
  assert_eq(original_counters.length(), deserialized_counters.length())
  
  let original_histograms = MetricsBatch::get_histograms(metrics_batch)
  let deserialized_histograms = MetricsBatch::get_histograms(deserialized_batch)
  assert_eq(original_histograms.length(), deserialized_histograms.length())
}

// Test 3: Protocol Buffer Serialization
test "protocol buffer serialization of telemetry data" {
  // Create a log record with context
  let attrs = Attributes::new()
  Attributes::set(attrs, "log.level", StringValue("INFO"))
  Attributes::set(attrs, "service.name", StringValue("test_service"))
  
  let log_record = LogRecord::new_with_context(
    Info,
    Some("Test log message"),
    Some(attrs),
    Some(1234567890L),
    Some(1234567891L),
    Some("trace_id_123"),
    Some("span_id_456"),
    Some(Context::root())
  )
  
  // Serialize to protocol buffer format
  let proto_data = ProtoSerializer::serialize_log_record(log_record)
  assert_true(proto_data.length() > 0)
  
  // Deserialize from protocol buffer format
  let deserialized_log = ProtoSerializer::deserialize_log_record(proto_data)
  
  // Verify deserialized data matches original
  assert_eq(LogRecord::severity_number(deserialized_log), LogRecord::severity_number(log_record))
  
  match LogRecord::body(deserialized_log) {
    Some(body) => {
      match LogRecord::body(log_record) {
        Some(original_body) => assert_eq(body, original_body)
        None => assert_true(false)
      }
    }
    None => assert_true(false)
  }
  
  assert_eq(LogRecord::trace_id(deserialized_log), LogRecord::trace_id(log_record))
  assert_eq(LogRecord::span_id(deserialized_log), LogRecord::span_id(log_record))
}

// Test 4: Cross-format Serialization Compatibility
test "cross-format serialization compatibility" {
  // Create a resource with attributes
  let resource = Resource::new()
  let attrs = [
    ("service.name", StringValue("test_service")),
    ("service.version", StringValue("1.0.0")),
    ("service.instance.id", StringValue("instance-123")),
    ("host.name", StringValue("test-host")),
    ("process.pid", IntValue(12345))
  ]
  let resource_with_attrs = Resource::with_attributes(resource, attrs)
  
  // Serialize to JSON
  let json_data = JsonSerializer::serialize_resource(resource_with_attrs)
  
  // Deserialize from JSON
  let json_deserialized = JsonSerializer::deserialize_resource(json_data)
  
  // Serialize the deserialized resource to binary
  let binary_data = BinarySerializer::serialize_resource(json_deserialized)
  
  // Deserialize from binary
  let binary_deserialized = BinarySerializer::deserialize_resource(binary_data)
  
  // Verify all attributes are preserved across formats
  let service_name = Resource::get_attribute(binary_deserialized, "service.name")
  match service_name {
    Some(StringValue(name)) => assert_eq(name, "test_service")
    _ => assert_true(false)
  }
  
  let service_version = Resource::get_attribute(binary_deserialized, "service.version")
  match service_version {
    Some(StringValue(version)) => assert_eq(version, "1.0.0")
    _ => assert_true(false)
  }
  
  let process_pid = Resource::get_attribute(binary_deserialized, "process.pid")
  match process_pid {
    Some(IntValue(pid)) => assert_eq(pid, 12345)
    _ => assert_true(false)
  }
}