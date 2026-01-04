// Azimuth Telemetry System - Enhanced Serialization Tests
// This file contains comprehensive test cases for telemetry data serialization and deserialization

// Test 1: Attribute Value Serialization
test "attribute value serialization" {
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
  let float_attr = FloatValue(3.14)
  let serialized_float = AttributeValue::serialize(float_attr)
  let deserialized_float = AttributeValue::deserialize(serialized_float)
  
  match deserialized_float {
    Some(FloatValue(v)) => assert_eq(v, 3.14)
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

// Test 2: Array Attribute Serialization
test "array attribute serialization" {
  // Test array string attribute serialization
  let array_string_attr = ArrayStringValue(["a", "b", "c"])
  let serialized_array_string = AttributeValue::serialize(array_string_attr)
  let deserialized_array_string = AttributeValue::deserialize(serialized_array_string)
  
  match deserialized_array_string {
    Some(ArrayStringValue(v)) => {
      assert_eq(v.length(), 3)
      assert_eq(v[0], "a")
      assert_eq(v[1], "b")
      assert_eq(v[2], "c")
    }
    _ => assert_true(false)
  }
  
  // Test array int attribute serialization
  let array_int_attr = ArrayIntValue([1, 2, 3, 4, 5])
  let serialized_array_int = AttributeValue::serialize(array_int_attr)
  let deserialized_array_int = AttributeValue::deserialize(serialized_array_int)
  
  match deserialized_array_int {
    Some(ArrayIntValue(v)) => {
      assert_eq(v.length(), 5)
      assert_eq(v[0], 1)
      assert_eq(v[4], 5)
    }
    _ => assert_true(false)
  }
  
  // Test empty array serialization
  let empty_array_attr = ArrayStringValue([])
  let serialized_empty = AttributeValue::serialize(empty_array_attr)
  let deserialized_empty = AttributeValue::deserialize(serialized_empty)
  
  match deserialized_empty {
    Some(ArrayStringValue(v)) => assert_eq(v.length(), 0)
    _ => assert_true(false)
  }
}

// Test 3: Attributes Map Serialization
test "attributes map serialization" {
  let attrs = Attributes::new()
  
  // Add various attribute types
  Attributes::set(attrs, "string.key", StringValue("test_value"))
  Attributes::set(attrs, "int.key", IntValue(42))
  Attributes::set(attrs, "float.key", FloatValue(3.14))
  Attributes::set(attrs, "bool.key", BoolValue(true))
  Attributes::set(attrs, "array.string.key", ArrayStringValue(["x", "y", "z"]))
  Attributes::set(attrs, "array.int.key", ArrayIntValue([10, 20, 30]))
  
  // Serialize attributes
  let serialized_attrs = Attributes::serialize(attrs)
  assert_true(serialized_attrs.length() > 0)
  
  // Deserialize attributes
  let deserialized_attrs = Attributes::deserialize(serialized_attrs)
  
  // Verify all attributes are correctly deserialized
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
  
  let array_string_result = Attributes::get(deserialized_attrs, "array.string.key")
  match array_string_result {
    Some(ArrayStringValue(v)) => {
      assert_eq(v.length(), 3)
      assert_eq(v[0], "x")
      assert_eq(v[2], "z")
    }
    _ => assert_true(false)
  }
  
  let array_int_result = Attributes::get(deserialized_attrs, "array.int.key")
  match array_int_result {
    Some(ArrayIntValue(v)) => {
      assert_eq(v.length(), 3)
      assert_eq(v[0], 10)
      assert_eq(v[2], 30)
    }
    _ => assert_true(false)
  }
}

// Test 4: Span Context Serialization
test "span context serialization" {
  let trace_id = "0af7651916cd43dd8448eb211c80319c"
  let span_id = "b7ad6b7169203331"
  let span_ctx = SpanContext::new(trace_id, span_id, true, "test_state")
  
  // Serialize span context
  let serialized_ctx = SpanContext::serialize(span_ctx)
  assert_true(serialized_ctx.length() > 0)
  
  // Deserialize span context
  let deserialized_ctx = SpanContext::deserialize(serialized_ctx)
  
  // Verify deserialized context
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
  Span::add_attribute(span, "custom.attr", StringValue("custom_value"))
  
  // Serialize span
  let serialized_span = Span::serialize(span)
  assert_true(serialized_span.length() > 0)
  
  // Deserialize span
  let deserialized_span = Span::deserialize(serialized_span)
  
  // Verify deserialized span
  assert_eq(Span::name(deserialized_span), "test_span")
  match Span::kind(deserialized_span) {
    Internal => assert_true(true)
    _ => assert_true(false)
  }
  
  let retrieved_ctx = Span::span_context(deserialized_span)
  assert_eq(SpanContext::trace_id(retrieved_ctx), "trace_id")
  assert_eq(SpanContext::span_id(retrieved_ctx), "span_id")
  
  assert_eq(Span::status(deserialized_span), Ok)
  
  // Verify custom attribute
  let custom_attr = Span::get_attribute(deserialized_span, "custom.attr")
  match custom_attr {
    Some(StringValue(v)) => assert_eq(v, "custom_value")
    _ => assert_true(false)
  }
}

// Test 6: Log Record Serialization
test "log record serialization" {
  let attrs = Attributes::new()
  Attributes::set(attrs, "log.level", StringValue("info"))
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
  
  // Serialize log record
  let serialized_log = LogRecord::serialize(log_record)
  assert_true(serialized_log.length() > 0)
  
  // Deserialize log record
  let deserialized_log = LogRecord::deserialize(serialized_log)
  
  // Verify deserialized log record
  assert_eq(LogRecord::severity_number(deserialized_log), Info)
  match LogRecord::body(deserialized_log) {
    Some(body) => assert_eq(body, "Test log message")
    None => assert_true(false)
  }
  
  assert_eq(LogRecord::trace_id(deserialized_log), Some("trace_id"))
  assert_eq(LogRecord::span_id(deserialized_log), Some("span_id"))
  
  // Verify log attributes
  let log_attrs = LogRecord::attributes(deserialized_log)
  let log_level = Attributes::get(log_attrs, "log.level")
  match log_level {
    Some(StringValue(v)) => assert_eq(v, "info")
    _ => assert_true(false)
  }
}

// Test 7: Resource Serialization
test "resource serialization" {
  let attrs = [
    ("service.name", StringValue("test_service")),
    ("service.version", StringValue("1.0.0")),
    ("service.instance.id", StringValue("instance-123")),
    ("host.name", StringValue("test-host")),
    ("process.id", IntValue(12345))
  ]
  let resource = Resource::with_attributes(Resource::new(), attrs)
  
  // Serialize resource
  let serialized_resource = Resource::serialize(resource)
  assert_true(serialized_resource.length() > 0)
  
  // Deserialize resource
  let deserialized_resource = Resource::deserialize(serialized_resource)
  
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
  
  let process_id = Resource::get_attribute(deserialized_resource, "process.id")
  match process_id {
    Some(IntValue(pid)) => assert_eq(pid, 12345)
    _ => assert_true(false)
  }
}

// Test 8: Metric Data Serialization
test "metric data serialization" {
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "test_meter")
  
  // Create and record metrics
  let counter = Meter::create_counter(meter, "test_counter", Some("Test counter"), Some("count"))
  Counter::add(counter, 1.0)
  Counter::add(counter, 2.5, Some(Attributes::new()))
  
  let histogram = Meter::create_histogram(meter, "test_histogram", Some("Test histogram"), Some("ms"))
  Histogram::record(histogram, 100.0)
  Histogram::record(histogram, 200.5, Some(Attributes::new()))
  
  // Get metric data
  let metric_data = Meter::collect(meter)
  
  // Serialize metric data
  let serialized_metrics = MetricData::serialize(metric_data)
  assert_true(serialized_metrics.length() > 0)
  
  // Deserialize metric data
  let deserialized_metrics = MetricData::deserialize(serialized_metrics)
  
  // Verify deserialized metrics
  let metrics = MetricData::metrics(deserialized_metrics)
  assert_true(metrics.length() >= 2) // At least counter and histogram
  
  // Find and verify counter metric
  let mut counter_found = false
  for metric in metrics {
    if Metric::name(metric) == "test_counter" {
      counter_found = true
      assert_eq(Metric::description(metric), Some("Test counter"))
      assert_eq(Metric::unit(metric), Some("count"))
      match Metric::data(metric) {
        CounterData(points) => {
          assert_true(points.length() > 0)
          // Verify sum of counter points
          let mut sum = 0.0
          for point in points {
            sum = sum + DataPoint::value(point)
          }
          assert_eq(sum, 3.5) // 1.0 + 2.5
        }
        _ => assert_true(false)
      }
    }
  }
  assert_true(counter_found)
}

// Test 9: Baggage Serialization
test "baggage serialization" {
  let baggage = Baggage::new()
  
  // Add baggage entries
  let updated_baggage = Baggage::set_entry(baggage, "user.id", "user123")
  let final_baggage = Baggage::set_entry(updated_baggage, "request.id", "req456")
  
  // Serialize baggage
  let serialized_baggage = Baggage::serialize(final_baggage)
  assert_true(serialized_baggage.length() > 0)
  
  // Deserialize baggage
  let deserialized_baggage = Baggage::deserialize(serialized_baggage)
  
  // Verify deserialized baggage entries
  let user_id = Baggage::get_entry(deserialized_baggage, "user.id")
  match user_id {
    Some(value) => assert_eq(value, "user123")
    None => assert_true(false)
  }
  
  let request_id = Baggage::get_entry(deserialized_baggage, "request.id")
  match request_id {
    Some(value) => assert_eq(value, "req456")
    None => assert_true(false)
  }
  
  // Verify non-existent entry
  let non_existent = Baggage::get_entry(deserialized_baggage, "non.existent")
  match non_existent {
    Some(_) => assert_true(false)
    None => assert_true(true)
  }
}

// Test 10: Complex Telemetry Scenario Serialization
test "complex telemetry scenario serialization" {
  // Create a complex telemetry scenario
  let trace_id = "complex_trace_12345"
  let span_ctx = SpanContext::new(trace_id, "span_123", true, "")
  let span = Span::new("complex_operation", Server, span_ctx)
  
  // Add span attributes and events
  Span::add_attribute(span, "operation.type", StringValue("data_processing"))
  Span::add_attribute(span, "operation.size", IntValue(1024))
  Span::add_event(span, "operation_started", Some([("timestamp", IntValue(1234567890))]))
  
  // Create log record
  let log_attrs = Attributes::new()
  Attributes::set(log_attrs, "component", StringValue("data_processor"))
  let log_record = LogRecord::new_with_context(
    Info,
    Some("Processing started"),
    Some(log_attrs),
    Some(1234567890L),
    Some(1234567891L),
    Some(trace_id),
    Some("span_123"),
    Some(Context::root())
  )
  
  // Create metrics
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "complex_meter")
  let counter = Meter::create_counter(meter, "operations_total", Some("Total operations"), Some("count"))
  Counter::add(counter, 1.0, Some([("operation.type", StringValue("data_processing"))]))
  
  // Collect all telemetry data
  let telemetry_batch = TelemetryBatch::new()
  TelemetryBatch::add_span(telemetry_batch, span)
  TelemetryBatch::add_log_record(telemetry_batch, log_record)
  TelemetryBatch::add_metrics(telemetry_batch, Meter::collect(meter))
  
  // Serialize the entire batch
  let serialized_batch = TelemetryBatch::serialize(telemetry_batch)
  assert_true(serialized_batch.length() > 0)
  
  // Deserialize the batch
  let deserialized_batch = TelemetryBatch::deserialize(serialized_batch)
  
  // Verify deserialized batch
  let spans = TelemetryBatch::spans(deserialized_batch)
  assert_eq(spans.length(), 1)
  
  let deserialized_span = spans[0]
  assert_eq(Span::name(deserialized_span), "complex_operation")
  match Span::kind(deserialized_span) {
    Server => assert_true(true)
    _ => assert_true(false)
  }
  
  let logs = TelemetryBatch::log_records(deserialized_batch)
  assert_eq(logs.length(), 1)
  
  let deserialized_log = logs[0]
  match LogRecord::body(deserialized_log) {
    Some(body) => assert_eq(body, "Processing started")
    None => assert_true(false)
  }
  
  let metrics = TelemetryBatch::metrics(deserialized_batch)
  assert_true(metrics.length() > 0)
}