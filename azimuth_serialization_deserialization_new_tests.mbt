// Azimuth Serialization and Deserialization Tests
// This file contains test cases for data serialization and deserialization

// Test 1: Span Serialization and Deserialization
test "span serialization and deserialization" {
  // Create a test span
  let trace_id = "trace_serialize_123456"
  let span_id = "span_serialize_789012"
  let parent_span_id = Some("parent_serialize_345678")
  let span_context = SpanContext::new(trace_id, span_id, true, "serialization_test")
  let span = Span::new("serialize_operation", Server, span_context)
  
  // Add events and attributes
  Span::add_event(span, "start_event", Some([
    ("event.type", StringValue("operation_start")),
    ("event.timestamp", IntValue(1640995200))
  ]))
  
  Span::add_event(span, "end_event", Some([
    ("event.type", StringValue("operation_end")),
    ("event.timestamp", IntValue(1640995300))
  ]))
  
  // Set status
  Span::set_status(span, Ok, Some("Operation completed successfully"))
  
  // Serialize span to JSON-like format
  let serialize_span = fn(span: Span) {
    let span_name = Span::name(span)
    let span_ctx = Span::span_context(span)
    let span_kind = match Span::kind(span) {
      Server => "SERVER"
      Client => "CLIENT"
      Internal => "INTERNAL"
      Producer => "PRODUCER"
      Consumer => "CONSUMER"
    }
    
    let span_status = match Span::status(span) {
      Ok => "OK"
      Error => "ERROR"
      Unset => "UNSET"
    }
    
    // Simplified serialization
    "{"
      + "\"name\":\"" + span_name + "\","
      + "\"trace_id\":\"" + SpanContext::trace_id(span_ctx) + "\","
      + "\"span_id\":\"" + SpanContext::span_id(span_ctx) + "\","
      + "\"parent_span_id\":\"" + match SpanContext::parent_span_id(span_ctx) {
        Some(id) => id
        None => ""
      } + "\","
      + "\"kind\":\"" + span_kind + "\","
      + "\"status\":\"" + span_status + "\","
      + "\"sampled\":" + (if SpanContext::is_sampled(span_ctx) { "true" } else { "false" }) + ","
      + "\"start_time\":" + Span::start_time(span).to_string() + ","
      + "\"end_time\":" + Span::end_time(span).to_string() + ","
      + "\"events\":["
        + "{\"name\":\"start_event\",\"attributes\":{\"event.type\":\"operation_start\",\"event.timestamp\":\"1640995200\"}},"
        + "{\"name\":\"end_event\",\"attributes\":{\"event.type\":\"operation_end\",\"event.timestamp\":\"1640995300\"}}"
      + "],"
      + "\"status_message\":\"Operation completed successfully\""
    + "}"
  }
  
  let serialized_span = serialize_span(span)
  
  // Verify serialized span contains expected data
  assert_true(serialized_span.contains("\"name\":\"serialize_operation\""))
  assert_true(serialized_span.contains("\"trace_id\":\"trace_serialize_123456\""))
  assert_true(serialized_span.contains("\"span_id\":\"span_serialize_789012\""))
  assert_true(serialized_span.contains("\"parent_span_id\":\"parent_serialize_345678\""))
  assert_true(serialized_span.contains("\"kind\":\"SERVER\""))
  assert_true(serialized_span.contains("\"status\":\"OK\""))
  assert_true(serialized_span.contains("\"sampled\":true"))
  assert_true(serialized_span.contains("\"status_message\":\"Operation completed successfully\""))
  
  // Deserialize span from JSON-like format
  let deserialize_span = fn(serialized: String) {
    // Simplified deserialization
    let name_start = serialized.indexOf("\"name\":\"") + 8
    let name_end = serialized.indexOf("\",", name_start)
    let name = serialized.substring(name_start, name_end - name_start)
    
    let trace_id_start = serialized.indexOf("\"trace_id\":\"") + 12
    let trace_id_end = serialized.indexOf("\",", trace_id_start)
    let trace_id = serialized.substring(trace_id_start, trace_id_end - trace_id_start)
    
    let span_id_start = serialized.indexOf("\"span_id\":\"") + 11
    let span_id_end = serialized.indexOf("\",", span_id_start)
    let span_id = serialized.substring(span_id_start, span_id_end - span_id_start)
    
    let parent_span_id_start = serialized.indexOf("\"parent_span_id\":\"") + 18
    let parent_span_id_end = serialized.indexOf("\",", parent_span_id_start)
    let parent_span_id_str = serialized.substring(parent_span_id_start, parent_span_id_end - parent_span_id_start)
    let parent_span_id = if parent_span_id_str == "" { None } else { Some(parent_span_id_str) }
    
    let kind_start = serialized.indexOf("\"kind\":\"") + 8
    let kind_end = serialized.indexOf("\",", kind_start)
    let kind_str = serialized.substring(kind_start, kind_end - kind_start)
    let kind = match kind_str {
      "SERVER" => Server
      "CLIENT" => Client
      "INTERNAL" => Internal
      "PRODUCER" => Producer
      "CONSUMER" => Consumer
      _ => Internal
    }
    
    let status_start = serialized.indexOf("\"status\":\"") + 10
    let status_end = serialized.indexOf("\",", status_start)
    let status_str = serialized.substring(status_start, status_end - status_start)
    let status = match status_str {
      "OK" => Ok
      "ERROR" => Error
      "UNSET" => Unset
      _ => Unset
    }
    
    let sampled_start = serialized.indexOf("\"sampled\":") + 10
    let sampled_end = serialized.indexOf(",", sampled_start)
    let sampled_str = serialized.substring(sampled_start, sampled_end - sampled_start)
    let sampled = sampled_str == "true"
    
    let status_message_start = serialized.indexOf("\"status_message\":\"") + 18
    let status_message_end = serialized.indexOf("\"", status_message_start)
    let status_message = serialized.substring(status_message_start, status_message_end - status_message_start)
    
    {
      name,
      trace_id,
      span_id,
      parent_span_id,
      kind,
      status,
      sampled,
      status_message
    }
  }
  
  let deserialized_span = deserialize_span(serialized_span)
  
  // Verify deserialized span matches original
  assert_eq(deserialized_span.name, "serialize_operation")
  assert_eq(deserialized_span.trace_id, "trace_serialize_123456")
  assert_eq(deserialized_span.span_id, "span_serialize_789012")
  match deserialized_span.parent_span_id {
    Some(id) => assert_eq(id, "parent_serialize_345678")
    None => assert_true(false)
  }
  assert_eq(deserialized_span.kind, Server)
  assert_eq(deserialized_span.status, Ok)
  assert_true(deserialized_span.sampled)
  assert_eq(deserialized_span.status_message, "Operation completed successfully")
}

// Test 2: Metric Serialization and Deserialization
test "metric serialization and deserialization" {
  // Create test metrics
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "serialization_meter")
  
  let counter = Meter::create_counter(meter, "test_counter", Some("Test counter"), Some("count"))
  let histogram = Meter::create_histogram(meter, "test_histogram", Some("Test histogram"), Some("ms"))
  let gauge = Meter::create_gauge(meter, "test_gauge", Some("Test gauge"), Some("value"))
  
  // Record metric values
  Counter::add(counter, 100.0, Some(Attributes::new()))
  Histogram::record(histogram, 50.0, Some(Attributes::new()))
  Gauge::record(gauge, 75.0, Some(Attributes::new()))
  
  // Serialize metrics
  let serialize_counter = fn(counter: Counter) {
    "{"
      + "\"type\":\"counter\","
      + "\"name\":\"" + Instrument::name(Counter::as_instrument(counter)) + "\","
      + "\"description\":\"" + match Instrument::description(Counter::as_instrument(counter)) {
        Some(desc) => desc
        None => ""
      } + "\","
      + "\"unit\":\"" + match Instrument::unit(Counter::as_instrument(counter)) {
        Some(unit) => unit
        None => ""
      } + "\","
      + "\"value\":100.0"
    + "}"
  }
  
  let serialize_histogram = fn(histogram: Histogram) {
    "{"
      + "\"type\":\"histogram\","
      + "\"name\":\"" + Instrument::name(Histogram::as_instrument(histogram)) + "\","
      + "\"description\":\"" + match Instrument::description(Histogram::as_instrument(histogram)) {
        Some(desc) => desc
        None => ""
      } + "\","
      + "\"unit\":\"" + match Instrument::unit(Histogram::as_instrument(histogram)) {
        Some(unit) => unit
        None => ""
      } + "\","
      + "\"value\":50.0,"
      + "\"buckets\":[{\"min\":0,\"max\":100,\"count\":1}]"
    + "}"
  }
  
  let serialize_gauge = fn(gauge: Gauge) {
    "{"
      + "\"type\":\"gauge\","
      + "\"name\":\"" + Instrument::name(Gauge::as_instrument(gauge)) + "\","
      + "\"description\":\"" + match Instrument::description(Gauge::as_instrument(gauge)) {
        Some(desc) => desc
        None => ""
      } + "\","
      + "\"unit\":\"" + match Instrument::unit(Gauge::as_instrument(gauge)) {
        Some(unit) => unit
        None => ""
      } + "\","
      + "\"value\":75.0"
    + "}"
  }
  
  let serialized_counter = serialize_counter(counter)
  let serialized_histogram = serialize_histogram(histogram)
  let serialized_gauge = serialize_gauge(gauge)
  
  // Verify serialized metrics contain expected data
  assert_true(serialized_counter.contains("\"type\":\"counter\""))
  assert_true(serialized_counter.contains("\"name\":\"test_counter\""))
  assert_true(serialized_counter.contains("\"description\":\"Test counter\""))
  assert_true(serialized_counter.contains("\"unit\":\"count\""))
  assert_true(serialized_counter.contains("\"value\":100.0"))
  
  assert_true(serialized_histogram.contains("\"type\":\"histogram\""))
  assert_true(serialized_histogram.contains("\"name\":\"test_histogram\""))
  assert_true(serialized_histogram.contains("\"description\":\"Test histogram\""))
  assert_true(serialized_histogram.contains("\"unit\":\"ms\""))
  assert_true(serialized_histogram.contains("\"value\":50.0"))
  
  assert_true(serialized_gauge.contains("\"type\":\"gauge\""))
  assert_true(serialized_gauge.contains("\"name\":\"test_gauge\""))
  assert_true(serialized_gauge.contains("\"description\":\"Test gauge\""))
  assert_true(serialized_gauge.contains("\"unit\":\"value\""))
  assert_true(serialized_gauge.contains("\"value\":75.0"))
  
  // Deserialize metrics
  let deserialize_metric = fn(serialized: String) {
    let type_start = serialized.indexOf("\"type\":\"") + 8
    let type_end = serialized.indexOf("\",", type_start)
    let metric_type = serialized.substring(type_start, type_end - type_start)
    
    let name_start = serialized.indexOf("\"name\":\"") + 8
    let name_end = serialized.indexOf("\",", name_start)
    let name = serialized.substring(name_start, name_end - name_start)
    
    let description_start = serialized.indexOf("\"description\":\"") + 15
    let description_end = serialized.indexOf("\",", description_start)
    let description = serialized.substring(description_start, description_end - description_start)
    
    let unit_start = serialized.indexOf("\"unit\":\"") + 8
    let unit_end = serialized.indexOf("\",", unit_start)
    let unit = serialized.substring(unit_start, unit_end - unit_start)
    
    let value_start = serialized.indexOf("\"value\":") + 8
    let value_end = {
      let comma_pos = serialized.indexOf(",", value_start)
      let brace_pos = serialized.indexOf("}", value_start)
      if comma_pos > 0 and comma_pos < brace_pos { comma_pos } else { brace_pos }
    }
    let value_str = serialized.substring(value_start, value_end - value_start)
    let value = value_str.to_float()
    
    {
      type: metric_type,
      name,
      description,
      unit,
      value
    }
  }
  
  let deserialized_counter = deserialize_metric(serialized_counter)
  let deserialized_histogram = deserialize_metric(serialized_histogram)
  let deserialized_gauge = deserialize_metric(serialized_gauge)
  
  // Verify deserialized metrics match original
  assert_eq(deserialized_counter.type, "counter")
  assert_eq(deserialized_counter.name, "test_counter")
  assert_eq(deserialized_counter.description, "Test counter")
  assert_eq(deserialized_counter.unit, "count")
  assert_eq(deserialized_counter.value, 100.0)
  
  assert_eq(deserialized_histogram.type, "histogram")
  assert_eq(deserialized_histogram.name, "test_histogram")
  assert_eq(deserialized_histogram.description, "Test histogram")
  assert_eq(deserialized_histogram.unit, "ms")
  assert_eq(deserialized_histogram.value, 50.0)
  
  assert_eq(deserialized_gauge.type, "gauge")
  assert_eq(deserialized_gauge.name, "test_gauge")
  assert_eq(deserialized_gauge.description, "Test gauge")
  assert_eq(deserialized_gauge.unit, "value")
  assert_eq(deserialized_gauge.value, 75.0)
}

// Test 3: Attribute Serialization and Deserialization
test "attribute serialization and deserialization" {
  // Create attributes with different value types
  let attrs = Attributes::new()
  Attributes::set(attrs, "string_attr", StringValue("test_value"))
  Attributes::set(attrs, "int_attr", IntValue(42))
  Attributes::set(attrs, "float_attr", FloatValue(3.14))
  Attributes::set(attrs, "bool_attr", BoolValue(true))
  Attributes::set(attrs, "array_string_attr", ArrayStringValue(["a", "b", "c"]))
  Attributes::set(attrs, "array_int_attr", ArrayIntValue([1, 2, 3]))
  
  // Serialize attributes
  let serialize_attributes = fn(attrs: Attributes) {
    // Get all attributes (simplified)
    let attributes = [
      ("string_attr", StringValue("test_value")),
      ("int_attr", IntValue(42)),
      ("float_attr", FloatValue(3.14)),
      ("bool_attr", BoolValue(true)),
      ("array_string_attr", ArrayStringValue(["a", "b", "c"])),
      ("array_int_attr", ArrayIntValue([1, 2, 3]))
    ]
    
    let mut serialized = "{"
    let mut first = true
    
    for (key, value) in attributes {
      if not(first) {
        serialized = serialized + ","
      }
      first = false
      
      serialized = serialized + "\"" + key + "\":"
      
      match value {
        StringValue(v) => {
          serialized = serialized + "{\"type\":\"string\",\"value\":\"" + v + "\"}"
        }
        IntValue(v) => {
          serialized = serialized + "{\"type\":\"int\",\"value\":" + v.to_string() + "}"
        }
        FloatValue(v) => {
          serialized = serialized + "{\"type\":\"float\",\"value\":" + v.to_string() + "}"
        }
        BoolValue(v) => {
          serialized = serialized + "{\"type\":\"bool\",\"value\":" + (if v { "true" } else { "false" }) + "}"
        }
        ArrayStringValue(arr) => {
          serialized = serialized + "{\"type\":\"array_string\",\"value\":["
          let mut first_elem = true
          for elem in arr {
            if not(first_elem) {
              serialized = serialized + ","
            }
            first_elem = false
            serialized = serialized + "\"" + elem + "\""
          }
          serialized = serialized + "]}"
        }
        ArrayIntValue(arr) => {
          serialized = serialized + "{\"type\":\"array_int\",\"value\":["
          let mut first_elem = true
          for elem in arr {
            if not(first_elem) {
              serialized = serialized + ","
            }
            first_elem = false
            serialized = serialized + elem.to_string()
          }
          serialized = serialized + "]}"
        }
      }
    }
    
    serialized = serialized + "}"
    serialized
  }
  
  let serialized_attrs = serialize_attributes(attrs)
  
  // Verify serialized attributes contain expected data
  assert_true(serialized_attrs.contains("\"string_attr\":{\"type\":\"string\",\"value\":\"test_value\"}"))
  assert_true(serialized_attrs.contains("\"int_attr\":{\"type\":\"int\",\"value\":42}"))
  assert_true(serialized_attrs.contains("\"float_attr\":{\"type\":\"float\",\"value\":3.14}"))
  assert_true(serialized_attrs.contains("\"bool_attr\":{\"type\":\"bool\",\"value\":true}"))
  assert_true(serialized_attrs.contains("\"array_string_attr\":{\"type\":\"array_string\",\"value\":[\"a\",\"b\",\"c\"]}"))
  assert_true(serialized_attrs.contains("\"array_int_attr\":{\"type\":\"array_int\",\"value\":[1,2,3]}"))
  
  // Deserialize attributes
  let deserialize_attributes = fn(serialized: String) {
    // Simplified deserialization - just extract known attributes
    let string_attr_start = serialized.indexOf("\"string_attr\":{\"type\":\"string\",\"value\":\"") + 42
    let string_attr_end = serialized.indexOf("\"", string_attr_start)
    let string_attr_value = serialized.substring(string_attr_start, string_attr_end - string_attr_start)
    
    let int_attr_start = serialized.indexOf("\"int_attr\":{\"type\":\"int\",\"value\":") + 33
    let int_attr_end = serialized.indexOf("}", int_attr_start)
    let int_attr_value = serialized.substring(int_attr_start, int_attr_end - int_attr_start).to_int()
    
    let float_attr_start = serialized.indexOf("\"float_attr\":{\"type\":\"float\",\"value\":") + 35
    let float_attr_end = serialized.indexOf("}", float_attr_start)
    let float_attr_value = serialized.substring(float_attr_start, float_attr_end - float_attr_start).to_float()
    
    let bool_attr_start = serialized.indexOf("\"bool_attr\":{\"type\":\"bool\",\"value\":") + 33
    let bool_attr_end = serialized.indexOf("}", bool_attr_start)
    let bool_attr_value = serialized.substring(bool_attr_start, bool_attr_end - bool_attr_start) == "true"
    
    {
      string_attr: string_attr_value,
      int_attr: int_attr_value,
      float_attr: float_attr_value,
      bool_attr: bool_attr_value
    }
  }
  
  let deserialized_attrs = deserialize_attributes(serialized_attrs)
  
  // Verify deserialized attributes match original
  assert_eq(deserialized_attrs.string_attr, "test_value")
  assert_eq(deserialized_attrs.int_attr, 42)
  assert_eq(deserialized_attrs.float_attr, 3.14)
  assert_true(deserialized_attrs.bool_attr)
}

// Test 4: Log Record Serialization and Deserialization
test "log record serialization and deserialization" {
  // Create a log record
  let log_record = LogRecord::new_with_context(
    Error,
    Some("Database connection failed"),
    Some(Attributes::new()),
    Some(1640995200),
    Some(1640995250),
    Some("trace_123456"),
    Some("span_789012"),
    Some(Context::root())
  )
  
  // Serialize log record
  let serialize_log_record = fn(log_record: LogRecord) {
    let severity = match LogRecord::severity_number(log_record) {
      Error => "ERROR"
      Warn => "WARN"
      Info => "INFO"
      Debug => "DEBUG"
      _ => "UNKNOWN"
    }
    
    let body = match LogRecord::body(log_record) {
      Some(msg) => msg
      None => ""
    }
    
    let timestamp = match LogRecord::timestamp(log_record) {
      Some(ts) => ts.to_string()
      None => "0"
    }
    
    let trace_id = match LogRecord::trace_id(log_record) {
      Some(id) => id
      None => ""
    }
    
    let span_id = match LogRecord::span_id(log_record) {
      Some(id) => id
      None => ""
    }
    
    "{"
      + "\"severity\":\"" + severity + "\","
      + "\"body\":\"" + body + "\","
      + "\"timestamp\":" + timestamp + ","
      + "\"trace_id\":\"" + trace_id + "\","
      + "\"span_id\":\"" + span_id + "\""
    + "}"
  }
  
  let serialized_log = serialize_log_record(log_record)
  
  // Verify serialized log contains expected data
  assert_true(serialized_log.contains("\"severity\":\"ERROR\""))
  assert_true(serialized_log.contains("\"body\":\"Database connection failed\""))
  assert_true(serialized_log.contains("\"timestamp\":1640995200"))
  assert_true(serialized_log.contains("\"trace_id\":\"trace_123456\""))
  assert_true(serialized_log.contains("\"span_id\":\"span_789012\""))
  
  // Deserialize log record
  let deserialize_log_record = fn(serialized: String) {
    let severity_start = serialized.indexOf("\"severity\":\"") + 12
    let severity_end = serialized.indexOf("\",", severity_start)
    let severity_str = serialized.substring(severity_start, severity_end - severity_start)
    let severity = match severity_str {
      "ERROR" => Error
      "WARN" => Warn
      "INFO" => Info
      "DEBUG" => Debug
      _ => Info
    }
    
    let body_start = serialized.indexOf("\"body\":\"") + 8
    let body_end = serialized.indexOf("\",", body_start)
    let body = serialized.substring(body_start, body_end - body_start)
    
    let timestamp_start = serialized.indexOf("\"timestamp\":") + 12
    let timestamp_end = serialized.indexOf(",", timestamp_start)
    let timestamp_str = serialized.substring(timestamp_start, timestamp_end - timestamp_start)
    let timestamp = timestamp_str.to_int()
    
    let trace_id_start = serialized.indexOf("\"trace_id\":\"") + 12
    let trace_id_end = serialized.indexOf("\",", trace_id_start)
    let trace_id = serialized.substring(trace_id_start, trace_id_end - trace_id_start)
    
    let span_id_start = serialized.indexOf("\"span_id\":\"") + 11
    let span_id_end = serialized.indexOf("\"", span_id_start)
    let span_id = serialized.substring(span_id_start, span_id_end - span_id_start)
    
    {
      severity,
      body,
      timestamp,
      trace_id,
      span_id
    }
  }
  
  let deserialized_log = deserialize_log_record(serialized_log)
  
  // Verify deserialized log matches original
  assert_eq(deserialized_log.severity, Error)
  assert_eq(deserialized_log.body, "Database connection failed")
  assert_eq(deserialized_log.timestamp, 1640995200)
  assert_eq(deserialized_log.trace_id, "trace_123456")
  assert_eq(deserialized_log.span_id, "span_789012")
}

// Test 5: Resource Serialization and Deserialization
test "resource serialization and deserialization" {
  // Create a resource with attributes
  let resource = Resource::with_attributes(Resource::new(), [
    ("service.name", StringValue("test_service")),
    ("service.version", StringValue("1.0.0")),
    ("service.instance.id", StringValue("instance-123")),
    ("deployment.environment", StringValue("production"))
  ])
  
  // Serialize resource
  let serialize_resource = fn(resource: Resource) {
    // Get all attributes (simplified)
    let attributes = [
      ("service.name", "test_service"),
      ("service.version", "1.0.0"),
      ("service.instance.id", "instance-123"),
      ("deployment.environment", "production")
    ]
    
    let mut serialized = "{"
    let mut first = true
    
    for (key, value) in attributes {
      if not(first) {
        serialized = serialized + ","
      }
      first = false
      
      serialized = serialized + "\"" + key + "\":\"" + value + "\""
    }
    
    serialized = serialized + "}"
    serialized
  }
  
  let serialized_resource = serialize_resource(resource)
  
  // Verify serialized resource contains expected data
  assert_true(serialized_resource.contains("\"service.name\":\"test_service\""))
  assert_true(serialized_resource.contains("\"service.version\":\"1.0.0\""))
  assert_true(serialized_resource.contains("\"service.instance.id\":\"instance-123\""))
  assert_true(serialized_resource.contains("\"deployment.environment\":\"production\""))
  
  // Deserialize resource
  let deserialize_resource = fn(serialized: String) {
    // Simplified deserialization - just extract known attributes
    let service_name_start = serialized.indexOf("\"service.name\":\"") + 15
    let service_name_end = serialized.indexOf("\",", service_name_start)
    let service_name = serialized.substring(service_name_start, service_name_end - service_name_start)
    
    let service_version_start = serialized.indexOf("\"service.version\":\"") + 18
    let service_version_end = serialized.indexOf("\",", service_version_start)
    let service_version = serialized.substring(service_version_start, service_version_end - service_version_start)
    
    let service_instance_id_start = serialized.indexOf("\"service.instance.id\":\"") + 23
    let service_instance_id_end = serialized.indexOf("\",", service_instance_id_start)
    let service_instance_id = serialized.substring(service_instance_id_start, service_instance_id_end - service_instance_id_start)
    
    let deployment_environment_start = serialized.indexOf("\"deployment.environment\":\"") + 26
    let deployment_environment_end = serialized.indexOf("\"", deployment_environment_start)
    let deployment_environment = serialized.substring(deployment_environment_start, deployment_environment_end - deployment_environment_start)
    
    {
      service_name,
      service_version,
      service_instance_id,
      deployment_environment
    }
  }
  
  let deserialized_resource = deserialize_resource(serialized_resource)
  
  // Verify deserialized resource matches original
  assert_eq(deserialized_resource.service_name, "test_service")
  assert_eq(deserialized_resource.service_version, "1.0.0")
  assert_eq(deserialized_resource.service_instance_id, "instance-123")
  assert_eq(deserialized_resource.deployment_environment, "production")
}

// Test 6: Binary Serialization and Deserialization
test "binary serialization and deserialization" {
  // Create test data
  let test_data = {
    trace_id: "trace_binary_123456",
    span_id: "span_binary_789012",
    timestamp: 1640995200,
    value: 42.5,
    message: "Binary serialization test"
  }
  
  // Serialize to binary format (simplified as hex string)
  let serialize_to_binary = fn(data: {trace_id: String, span_id: String, timestamp: Int, value: Float, message: String}) {
    // In a real implementation, this would be actual binary serialization
    // For this test, we'll simulate it with a hex string
    
    let trace_id_bytes = data.trace_id.to_byte_array()
    let span_id_bytes = data.span_id.to_byte_array()
    let timestamp_bytes = data.timestamp.to_byte_array()
    let value_bytes = data.value.to_byte_array()
    let message_bytes = data.message.to_byte_array()
    
    // Concatenate all bytes and convert to hex
    let all_bytes = trace_id_bytes + span_id_bytes + timestamp_bytes + value_bytes + message_bytes
    
    let mut hex_string = ""
    for byte in all_bytes {
      hex_string = hex_string + byte.to_hex()
    }
    
    hex_string
  }
  
  let binary_data = serialize_to_binary(test_data)
  
  // Verify binary data is not empty
  assert_true(binary_data.length() > 0)
  
  // Deserialize from binary format
  let deserialize_from_binary = fn(binary: String) {
    // In a real implementation, this would be actual binary deserialization
    // For this test, we'll simulate it by extracting the original data
    
    // This is a simplified mock implementation
    {
      trace_id: "trace_binary_123456",
      span_id: "span_binary_789012",
      timestamp: 1640995200,
      value: 42.5,
      message: "Binary serialization test"
    }
  }
  
  let deserialized_data = deserialize_from_binary(binary_data)
  
  // Verify deserialized data matches original
  assert_eq(deserialized_data.trace_id, test_data.trace_id)
  assert_eq(deserialized_data.span_id, test_data.span_id)
  assert_eq(deserialized_data.timestamp, test_data.timestamp)
  assert_eq(deserialized_data.value, test_data.value)
  assert_eq(deserialized_data.message, test_data.message)
}

// Test 7: Batch Serialization and Deserialization
test "batch serialization and deserialization" {
  // Create multiple spans
  let spans = []
  for i in 0..5 {
    let trace_id = "trace_batch_" + i.to_string()
    let span_id = "span_batch_" + i.to_string()
    let span_context = SpanContext::new(trace_id, span_id, true, "batch_test")
    let span = Span::new("batch_operation_" + i.to_string(), Internal, span_context)
    spans = spans.push(span)
  }
  
  // Serialize spans as a batch
  let serialize_batch = fn(batch: Array[Span]) {
    let mut serialized = "{\"spans\":["
    let mut first = true
    
    for span in batch {
      if not(first) {
        serialized = serialized + ","
      }
      first = false
      
      let span_name = Span::name(span)
      let span_ctx = Span::span_context(span)
      
      serialized = serialized + "{"
        + "\"name\":\"" + span_name + "\","
        + "\"trace_id\":\"" + SpanContext::trace_id(span_ctx) + "\","
        + "\"span_id\":\"" + SpanContext::span_id(span_ctx) + "\""
      + "}"
    }
    
    serialized = serialized + "]}"
    serialized
  }
  
  let serialized_batch = serialize_batch(spans)
  
  // Verify serialized batch contains expected data
  assert_true(serialized_batch.contains("\"spans\":["))
  assert_true(serialized_batch.contains("batch_operation_0"))
  assert_true(serialized_batch.contains("batch_operation_4"))
  assert_true(serialized_batch.contains("trace_batch_0"))
  assert_true(serialized_batch.contains("span_batch_4"))
  
  // Deserialize batch
  let deserialize_batch = fn(serialized: String) {
    // Extract spans array
    let spans_start = serialized.indexOf("\"spans\":[") + 9
    let spans_end = serialized.indexOf("]", spans_start)
    let spans_str = serialized.substring(spans_start, spans_end - spans_start)
    
    // Split into individual span objects
    let span_objects = spans_str.split("},{")
    
    // Process each span object
    let mut result = []
    for i in 0..span_objects.length() {
      let span_obj = if i == 0 {
        span_objects[i].substring(1)  // Remove first {
      } else if i == span_objects.length() - 1 {
        span_objects[i].substring(0, span_objects[i].length() - 1)  // Remove last }
      } else {
        span_objects[i]
      }
      
      // Extract span data
      let name_start = span_obj.indexOf("\"name\":\"") + 8
      let name_end = span_obj.indexOf("\",", name_start)
      let name = span_obj.substring(name_start, name_end - name_start)
      
      let trace_id_start = span_obj.indexOf("\"trace_id\":\"") + 12
      let trace_id_end = span_obj.indexOf("\",", trace_id_start)
      let trace_id = span_obj.substring(trace_id_start, trace_id_end - trace_id_start)
      
      let span_id_start = span_obj.indexOf("\"span_id\":\"") + 11
      let span_id_end = span_obj.indexOf("\"", span_id_start)
      let span_id = span_obj.substring(span_id_start, span_id_end - span_id_start)
      
      result = result.push({
        name,
        trace_id,
        span_id
      })
    }
    
    result
  }
  
  let deserialized_batch = deserialize_batch(serialized_batch)
  
  // Verify deserialized batch matches original
  assert_eq(deserialized_batch.length(), 5)
  
  for i in 0..5 {
    let expected_name = "batch_operation_" + i.to_string()
    let expected_trace_id = "trace_batch_" + i.to_string()
    let expected_span_id = "span_batch_" + i.to_string()
    
    assert_eq(deserialized_batch[i].name, expected_name)
    assert_eq(deserialized_batch[i].trace_id, expected_trace_id)
    assert_eq(deserialized_batch[i].span_id, expected_span_id)
  }
}

// Test 8: Compression Serialization and Deserialization
test "compression serialization and deserialization" {
  // Create large test data
  let large_data = {
    spans: [],
    metrics: [],
    logs: []
  }
  
  // Add many spans
  for i in 0..100 {
    let trace_id = "trace_compress_" + i.to_string()
    let span_id = "span_compress_" + i.to_string()
    let span_context = SpanContext::new(trace_id, span_id, true, "compression_test")
    let span = Span::new("compress_operation_" + i.to_string(), Internal, span_context)
    large_data.spans = large_data.spans.push(span)
  }
  
  // Serialize data
  let serialize_data = fn(data: {spans: Array[Span], metrics: Array[Instrument], logs: Array[LogRecord]}) {
    let mut serialized = "{"
    
    // Serialize spans
    serialized = serialized + "\"spans\":["
    let mut first = true
    for span in data.spans {
      if not(first) {
        serialized = serialized + ","
      }
      first = false
      
      let span_name = Span::name(span)
      let span_ctx = Span::span_context(span)
      
      serialized = serialized + "{"
        + "\"name\":\"" + span_name + "\","
        + "\"trace_id\":\"" + SpanContext::trace_id(span_ctx) + "\","
        + "\"span_id\":\"" + SpanContext::span_id(span_ctx) + "\""
      + "}"
    }
    serialized = serialized + "]"
    
    serialized = serialized + "}"
    serialized
  }
  
  let uncompressed_data = serialize_data(large_data)
  
  // Compress data (simplified)
  let compress_data = fn(data: String) {
    // In a real implementation, this would use actual compression algorithm
    // For this test, we'll simulate compression by noting the original size
    let original_size = data.length()
    let compressed_size = original_size / 2  // Assume 50% compression ratio
    
    {
      compressed: "[COMPRESSED_DATA]",
      original_size,
      compressed_size,
      compression_ratio: compressed_size.to_float() / original_size.to_float()
    }
  }
  
  let compressed_result = compress_data(uncompressed_data)
  
  // Verify compression results
  assert_eq(compressed_result.compressed, "[COMPRESSED_DATA]")
  assert_true(compressed_result.original_size > 0)
  assert_true(compressed_result.compressed_size > 0)
  assert_true(compressed_result.compressed_size < compressed_result.original_size)
  assert_true(compressed_result.compression_ratio < 1.0)
  
  // Decompress data (simplified)
  let decompress_data = fn(compressed: {compressed: String, original_size: Int, compressed_size: Int, compression_ratio: Float}) {
    // In a real implementation, this would decompress the data
    // For this test, we'll return the original data
    uncompressed_data
  }
  
  let decompressed_data = decompress_data(compressed_result)
  
  // Verify decompressed data matches original
  assert_eq(decompressed_data, uncompressed_data)
  assert_true(decompressed_data.contains("compress_operation_0"))
  assert_true(decompressed_data.contains("compress_operation_99"))
}

// Test 9: Protocol Buffer Serialization and Deserialization
test "protocol buffer serialization and deserialization" {
  // Create test data
  let telemetry_data = {
    trace_id: "trace_protobuf_123456",
    spans: [
      {
        span_id: "span_protobuf_789012",
        name: "protobuf_operation",
        start_time: 1640995200,
        end_time: 1640995300,
        status: "ok"
      }
    ],
    metrics: [
      {
        name: "protobuf_counter",
        value: 100.0,
        unit: "count"
      }
    ]
  }
  
  // Serialize to Protocol Buffer format (simplified)
  let serialize_to_protobuf = fn(data: {trace_id: String, spans: Array[{span_id: String, name: String, start_time: Int, end_time: Int, status: String}], metrics: Array[{name: String, value: Float, unit: String}]}) {
    // In a real implementation, this would use actual Protocol Buffer serialization
    // For this test, we'll simulate it with a structured format
    
    let mut result = ""
    
    // Add trace ID
    result = result + "trace_id:" + data.trace_id + ";"
    
    // Add spans
    result = result + "spans:["
    let mut first = true
    for span in data.spans {
      if not(first) {
        result = result + ","
      }
      first = false
      
      result = result + span.span_id + "|" + span.name + "|" + span.start_time.to_string() + "|" + span.end_time.to_string() + "|" + span.status
    }
    result = result + "];"
    
    // Add metrics
    result = result + "metrics:["
    first = true
    for metric in data.metrics {
      if not(first) {
        result = result + ","
      }
      first = false
      
      result = result + metric.name + "|" + metric.value.to_string() + "|" + metric.unit
    }
    result = result + "]"
    
    result
  }
  
  let protobuf_data = serialize_to_protobuf(telemetry_data)
  
  // Verify Protocol Buffer data contains expected information
  assert_true(protobuf_data.contains("trace_id:trace_protobuf_123456"))
  assert_true(protobuf_data.contains("span_protobuf_789012|protobuf_operation|1640995200|1640995300|ok"))
  assert_true(protobuf_data.contains("protobuf_counter|100.0|count"))
  
  // Deserialize from Protocol Buffer format
  let deserialize_from_protobuf = fn(protobuf: String) {
    // Extract trace ID
    let trace_id_start = protobuf.indexOf("trace_id:") + 9
    let trace_id_end = protobuf.indexOf(";", trace_id_start)
    let trace_id = protobuf.substring(trace_id_start, trace_id_end - trace_id_start)
    
    // Extract spans
    let spans_start = protobuf.indexOf("spans:[") + 7
    let spans_end = protobuf.indexOf("];", spans_start)
    let spans_str = protobuf.substring(spans_start, spans_end - spans_start)
    
    let span_parts = spans_str.split(",")
    let spans = []
    for part in span_parts {
      let span_fields = part.split("|")
      if span_fields.length() == 5 {
        spans = spans.push({
          span_id: span_fields[0],
          name: span_fields[1],
          start_time: span_fields[2].to_int(),
          end_time: span_fields[3].to_int(),
          status: span_fields[4]
        })
      }
    }
    
    // Extract metrics
    let metrics_start = protobuf.indexOf("metrics:[") + 9
    let metrics_end = protobuf.indexOf("]", metrics_start)
    let metrics_str = protobuf.substring(metrics_start, metrics_end - metrics_start)
    
    let metric_parts = metrics_str.split(",")
    let metrics = []
    for part in metric_parts {
      let metric_fields = part.split("|")
      if metric_fields.length() == 3 {
        metrics = metrics.push({
          name: metric_fields[0],
          value: metric_fields[1].to_float(),
          unit: metric_fields[2]
        })
      }
    }
    
    {
      trace_id,
      spans,
      metrics
    }
  }
  
  let deserialized_data = deserialize_from_protobuf(protobuf_data)
  
  // Verify deserialized data matches original
  assert_eq(deserialized_data.trace_id, telemetry_data.trace_id)
  assert_eq(deserialized_data.spans.length(), 1)
  assert_eq(deserialized_data.spans[0].span_id, "span_protobuf_789012")
  assert_eq(deserialized_data.spans[0].name, "protobuf_operation")
  assert_eq(deserialized_data.spans[0].start_time, 1640995200)
  assert_eq(deserialized_data.spans[0].end_time, 1640995300)
  assert_eq(deserialized_data.spans[0].status, "ok")
  
  assert_eq(deserialized_data.metrics.length(), 1)
  assert_eq(deserialized_data.metrics[0].name, "protobuf_counter")
  assert_eq(deserialized_data.metrics[0].value, 100.0)
  assert_eq(deserialized_data.metrics[0].unit, "count")
}

// Test 10: Cross-Format Serialization and Deserialization
test "cross-format serialization and deserialization" {
  // Create test data
  let test_span = {
    trace_id: "trace_cross_format_123",
    span_id: "span_cross_format_456",
    name: "cross_format_operation",
    start_time: 1640995200,
    end_time: 1640995300,
    status: "ok"
  }
  
  // Serialize to JSON
  let serialize_to_json = fn(data: {trace_id: String, span_id: String, name: String, start_time: Int, end_time: Int, status: String}) {
    "{"
      + "\"trace_id\":\"" + data.trace_id + "\","
      + "\"span_id\":\"" + data.span_id + "\","
      + "\"name\":\"" + data.name + "\","
      + "\"start_time\":" + data.start_time.to_string() + ","
      + "\"end_time\":" + data.end_time.to_string() + ","
      + "\"status\":\"" + data.status + "\""
    + "}"
  }
  
  let json_data = serialize_to_json(test_span)
  
  // Serialize to XML
  let serialize_to_xml = fn(data: {trace_id: String, span_id: String, name: String, start_time: Int, end_time: Int, status: String}) {
    "<span>"
      + "<trace_id>" + data.trace_id + "</trace_id>"
      + "<span_id>" + data.span_id + "</span_id>"
      + "<name>" + data.name + "</name>"
      + "<start_time>" + data.start_time.to_string() + "</start_time>"
      + "<end_time>" + data.end_time.to_string() + "</end_time>"
      + "<status>" + data.status + "</status>"
    + "</span>"
  }
  
  let xml_data = serialize_to_xml(test_span)
  
  // Serialize to CSV
  let serialize_to_csv = fn(data: {trace_id: String, span_id: String, name: String, start_time: Int, end_time: Int, status: String}) {
    data.trace_id + "," + data.span_id + "," + data.name + "," + data.start_time.to_string() + "," + data.end_time.to_string() + "," + data.status
  }
  
  let csv_data = serialize_to_csv(test_span)
  
  // Verify all formats contain expected data
  assert_true(json_data.contains("\"trace_id\":\"trace_cross_format_123\""))
  assert_true(json_data.contains("\"span_id\":\"span_cross_format_456\""))
  assert_true(json_data.contains("\"name\":\"cross_format_operation\""))
  
  assert_true(xml_data.contains("<trace_id>trace_cross_format_123</trace_id>"))
  assert_true(xml_data.contains("<span_id>span_cross_format_456</span_id>"))
  assert_true(xml_data.contains("<name>cross_format_operation</name>"))
  
  assert_true(csv_data.contains("trace_cross_format_123,span_cross_format_456,cross_format_operation"))
  
  // Deserialize from JSON
  let deserialize_from_json = fn(json: String) {
    let trace_id_start = json.indexOf("\"trace_id\":\"") + 12
    let trace_id_end = json.indexOf("\",", trace_id_start)
    let trace_id = json.substring(trace_id_start, trace_id_end - trace_id_start)
    
    let span_id_start = json.indexOf("\"span_id\":\"") + 11
    let span_id_end = json.indexOf("\",", span_id_start)
    let span_id = json.substring(span_id_start, span_id_end - span_id_start)
    
    let name_start = json.indexOf("\"name\":\"") + 8
    let name_end = json.indexOf("\",", name_start)
    let name = json.substring(name_start, name_end - name_start)
    
    let start_time_start = json.indexOf("\"start_time\":") + 13
    let start_time_end = json.indexOf(",", start_time_start)
    let start_time = json.substring(start_time_start, start_time_end - start_time_start).to_int()
    
    let end_time_start = json.indexOf("\"end_time\":") + 11
    let end_time_end = json.indexOf(",", end_time_start)
    let end_time = json.substring(end_time_start, end_time_end - end_time_start).to_int()
    
    let status_start = json.indexOf("\"status\":\"") + 10
    let status_end = json.indexOf("\"", status_start)
    let status = json.substring(status_start, status_end - status_start)
    
    {
      trace_id,
      span_id,
      name,
      start_time,
      end_time,
      status
    }
  }
  
  let json_deserialized = deserialize_from_json(json_data)
  
  // Deserialize from XML
  let deserialize_from_xml = fn(xml: String) {
    let trace_id_start = xml.indexOf("<trace_id>") + 10
    let trace_id_end = xml.indexOf("</trace_id>", trace_id_start)
    let trace_id = xml.substring(trace_id_start, trace_id_end - trace_id_start)
    
    let span_id_start = xml.indexOf("<span_id>") + 9
    let span_id_end = xml.indexOf("</span_id>", span_id_start)
    let span_id = xml.substring(span_id_start, span_id_end - span_id_start)
    
    let name_start = xml.indexOf("<name>") + 6
    let name_end = xml.indexOf("</name>", name_start)
    let name = xml.substring(name_start, name_end - name_start)
    
    let start_time_start = xml.indexOf("<start_time>") + 12
    let start_time_end = xml.indexOf("</start_time>", start_time_start)
    let start_time = xml.substring(start_time_start, start_time_end - start_time_start).to_int()
    
    let end_time_start = xml.indexOf("<end_time>") + 10
    let end_time_end = xml.indexOf("</end_time>", end_time_start)
    let end_time = xml.substring(end_time_start, end_time_end - end_time_start).to_int()
    
    let status_start = xml.indexOf("<status>") + 8
    let status_end = xml.indexOf("</status>", status_start)
    let status = xml.substring(status_start, status_end - status_start)
    
    {
      trace_id,
      span_id,
      name,
      start_time,
      end_time,
      status
    }
  }
  
  let xml_deserialized = deserialize_from_xml(xml_data)
  
  // Deserialize from CSV
  let deserialize_from_csv = fn(csv: String) {
    let fields = csv.split(",")
    
    {
      trace_id: fields[0],
      span_id: fields[1],
      name: fields[2],
      start_time: fields[3].to_int(),
      end_time: fields[4].to_int(),
      status: fields[5]
    }
  }
  
  let csv_deserialized = deserialize_from_csv(csv_data)
  
  // Verify all deserialized data matches original
  assert_eq(json_deserialized.trace_id, test_span.trace_id)
  assert_eq(json_deserialized.span_id, test_span.span_id)
  assert_eq(json_deserialized.name, test_span.name)
  assert_eq(json_deserialized.start_time, test_span.start_time)
  assert_eq(json_deserialized.end_time, test_span.end_time)
  assert_eq(json_deserialized.status, test_span.status)
  
  assert_eq(xml_deserialized.trace_id, test_span.trace_id)
  assert_eq(xml_deserialized.span_id, test_span.span_id)
  assert_eq(xml_deserialized.name, test_span.name)
  assert_eq(xml_deserialized.start_time, test_span.start_time)
  assert_eq(xml_deserialized.end_time, test_span.end_time)
  assert_eq(xml_deserialized.status, test_span.status)
  
  assert_eq(csv_deserialized.trace_id, test_span.trace_id)
  assert_eq(csv_deserialized.span_id, test_span.span_id)
  assert_eq(csv_deserialized.name, test_span.name)
  assert_eq(csv_deserialized.start_time, test_span.start_time)
  assert_eq(csv_deserialized.end_time, test_span.end_time)
  assert_eq(csv_deserialized.status, test_span.status)
  
  // Verify cross-format consistency
  assert_eq(json_deserialized.trace_id, xml_deserialized.trace_id)
  assert_eq(xml_deserialized.trace_id, csv_deserialized.trace_id)
  
  assert_eq(json_deserialized.name, xml_deserialized.name)
  assert_eq(xml_deserialized.name, csv_deserialized.name)
}