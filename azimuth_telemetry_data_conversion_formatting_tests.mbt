// Azimuth Telemetry System - Data Conversion and Formatting Tests
// This file contains comprehensive test cases for telemetry data conversion and formatting

// Test 1: Attribute Value Type Conversion Tests
test "attribute value type conversion" {
  // Test string to int conversion
  let string_int = StringValue("42")
  let converted_int = AttributeValue::to_int(string_int)
  match converted_int {
    Some(i) => assert_eq(i, 42)
    None => assert_true(false)
  }
  
  // Test string to float conversion
  let string_float = StringValue("3.14")
  let converted_float = AttributeValue::to_float(string_float)
  match converted_float {
    Some(f) => assert_eq(f, 3.14)
    None => assert_true(false)
  }
  
  // Test string to bool conversion
  let string_bool_true = StringValue("true")
  let converted_bool_true = AttributeValue::to_bool(string_bool_true)
  match converted_bool_true {
    Some(b) => assert_true(b)
    None => assert_true(false)
  }
  
  let string_bool_false = StringValue("false")
  let converted_bool_false = AttributeValue::to_bool(string_bool_false)
  match converted_bool_false {
    Some(b) => assert_false(b)
    None => assert_true(false)
  }
  
  // Test int to string conversion
  let int_value = IntValue(123)
  let converted_string = AttributeValue::to_string(int_value)
  match converted_string {
    Some(s) => assert_eq(s, "123")
    None => assert_true(false)
  }
  
  // Test float to string conversion
  let float_value = FloatValue(2.718)
  let converted_float_string = AttributeValue::to_string(float_value)
  match converted_float_string {
    Some(s) => assert_eq(s, "2.718")
    None => assert_true(false)
  }
  
  // Test bool to string conversion
  let bool_value = BoolValue(true)
  let converted_bool_string = AttributeValue::to_string(bool_value)
  match converted_bool_string {
    Some(s) => assert_eq(s, "true")
    None => assert_true(false)
  }
}

// Test 2: JSON Serialization and Deserialization
test "json serialization and deserialization" {
  // Create test attributes
  let attrs = Attributes::new()
  Attributes::set(attrs, "service.name", StringValue("test-service"))
  Attributes::set(attrs, "service.version", StringValue("1.0.0"))
  Attributes::set(attrs, "port", IntValue(8080))
  Attributes::set(attrs, "enabled", BoolValue(true))
  Attributes::set(attrs, "weight", FloatValue(0.95))
  
  // Serialize to JSON
  let json_str = Attributes::to_json(attrs)
  
  // Verify JSON contains expected fields
  assert_true(json_str.contains("\"service.name\""))
  assert_true(json_str.contains("\"test-service\""))
  assert_true(json_str.contains("\"service.version\""))
  assert_true(json_str.contains("\"1.0.0\""))
  assert_true(json_str.contains("\"port\""))
  assert_true(json_str.contains("8080"))
  assert_true(json_str.contains("\"enabled\""))
  assert_true(json_str.contains("true"))
  assert_true(json_str.contains("\"weight\""))
  assert_true(json_str.contains("0.95"))
  
  // Deserialize from JSON
  let deserialized_attrs = Attributes::from_json(json_str)
  
  // Verify deserialized attributes
  let service_name = Attributes::get(deserialized_attrs, "service.name")
  match service_name {
    Some(StringValue(name)) => assert_eq(name, "test-service")
    _ => assert_true(false)
  }
  
  let service_version = Attributes::get(deserialized_attrs, "service.version")
  match service_version {
    Some(StringValue(version)) => assert_eq(version, "1.0.0")
    _ => assert_true(false)
  }
  
  let port = Attributes::get(deserialized_attrs, "port")
  match port {
    Some(IntValue(p)) => assert_eq(p, 8080)
    _ => assert_true(false)
  }
  
  let enabled = Attributes::get(deserialized_attrs, "enabled")
  match enabled {
    Some(BoolValue(e)) => assert_true(e)
    _ => assert_true(false)
  }
  
  let weight = Attributes::get(deserialized_attrs, "weight")
  match weight {
    Some(FloatValue(w)) => assert_eq(w, 0.95)
    _ => assert_true(false)
  }
}

// Test 3: XML Serialization and Deserialization
test "xml serialization and deserialization" {
  // Create test span context
  let span_ctx = SpanContext::new("trace-123", "span-456", true, "test-state")
  
  // Serialize to XML
  let xml_str = SpanContext::to_xml(span_ctx)
  
  // Verify XML contains expected fields
  assert_true(xml_str.contains("<trace_id>trace-123</trace_id>"))
  assert_true(xml_str.contains("<span_id>span-456</span_id>"))
  assert_true(xml_str.contains("<sampled>true</sampled>"))
  assert_true(xml_str.contains("<trace_state>test-state</trace_state>"))
  
  // Deserialize from XML
  let deserialized_ctx = SpanContext::from_xml(xml_str)
  
  // Verify deserialized context
  assert_eq(SpanContext::trace_id(deserialized_ctx), "trace-123")
  assert_eq(SpanContext::span_id(deserialized_ctx), "span-456")
  assert_true(SpanContext::is_sampled(deserialized_ctx))
  assert_eq(SpanContext::trace_state(deserialized_ctx), "test-state")
}

// Test 4: CSV Formatting and Parsing
test "csv formatting and parsing" {
  // Create test metrics data
  let metrics_data = [
    ("timestamp", "metric_name", "value", "tags"),
    ("2023-01-01T00:00:00Z", "cpu.usage", "75.5", "host:server1,region:us-east"),
    ("2023-01-01T00:01:00Z", "memory.usage", "62.3", "host:server1,region:us-east"),
    ("2023-01-01T00:02:00Z", "disk.usage", "45.8", "host:server1,region:us-east")
  ]
  
  // Format to CSV
  let csv_str = Metrics::to_csv(metrics_data)
  
  // Verify CSV format
  assert_true(csv_str.contains("timestamp,metric_name,value,tags"))
  assert_true(csv_str.contains("2023-01-01T00:00:00Z,cpu.usage,75.5,host:server1,region:us-east"))
  assert_true(csv_str.contains("2023-01-01T00:01:00Z,memory.usage,62.3,host:server1,region:us-east"))
  assert_true(csv_str.contains("2023-01-01T00:02:00Z,disk.usage,45.8,host:server1,region:us-east"))
  
  // Parse from CSV
  let parsed_data = Metrics::from_csv(csv_str)
  
  // Verify parsed data
  assert_eq(parsed_data.length(), 4) // Header + 3 data rows
  
  // Check first data row
  let first_row = parsed_data[1]
  assert_eq(first_row[0], "2023-01-01T00:00:00Z")
  assert_eq(first_row[1], "cpu.usage")
  assert_eq(first_row[2], "75.5")
  assert_eq(first_row[3], "host:server1,region:us-east")
  
  // Check last data row
  let last_row = parsed_data[3]
  assert_eq(last_row[0], "2023-01-01T00:02:00Z")
  assert_eq(last_row[1], "disk.usage")
  assert_eq(last_row[2], "45.8")
  assert_eq(last_row[3], "host:server1,region:us-east")
}

// Test 5: Protocol Buffers Serialization
test "protocol buffers serialization" {
  // Create test log record
  let log_record = LogRecord::new(
    Error,
    "Test error message",
    Some([("error.code", IntValue(500)), ("error.type", StringValue("InternalError"))])
  )
  
  // Serialize to protocol buffers
  let proto_bytes = LogRecord::to_proto(log_record)
  
  // Verify proto bytes are not empty
  assert_true(proto_bytes.length() > 0)
  
  // Deserialize from protocol buffers
  let deserialized_log = LogRecord::from_proto(proto_bytes)
  
  // Verify deserialized log record
  assert_eq(LogRecord::severity_number(deserialized_log), Error)
  match LogRecord::body(deserialized_log) {
    Some(body) => assert_eq(body, "Test error message")
    None => assert_true(false)
  }
  
  // Verify attributes
  let error_code = LogRecord::get_attribute(deserialized_log, "error.code")
  match error_code {
    Some(IntValue(code)) => assert_eq(code, 500)
    _ => assert_true(false)
  }
  
  let error_type = LogRecord::get_attribute(deserialized_log, "error.type")
  match error_type {
    Some(StringValue(type_str)) => assert_eq(type_str, "InternalError")
    _ => assert_true(false)
  }
}

// Test 6: MessagePack Serialization
test "messagepack serialization" {
  // Create test resource
  let resource = Resource::with_attributes(Resource::new(), [
    ("service.name", StringValue("test-service")),
    ("service.instance.id", StringValue("instance-123")),
    ("service.namespace", StringValue("production")),
    ("service.version", StringValue("2.1.0")),
    ("host.name", StringValue("host-001")),
    ("host.ip", StringValue("192.168.1.100")),
    ("os.type", StringValue("linux")),
    ("os.version", StringValue("5.15.0")),
    ("process.pid", IntValue(12345)),
    ("process.executable.name", StringValue("/usr/bin/test-service"))
  ])
  
  // Serialize to MessagePack
  let msgpack_bytes = Resource::to_msgpack(resource)
  
  // Verify MessagePack bytes are not empty
  assert_true(msgpack_bytes.length() > 0)
  
  // Deserialize from MessagePack
  let deserialized_resource = Resource::from_msgpack(msgpack_bytes)
  
  // Verify deserialized resource attributes
  let service_name = Resource::get_attribute(deserialized_resource, "service.name")
  match service_name {
    Some(StringValue(name)) => assert_eq(name, "test-service")
    _ => assert_true(false)
  }
  
  let service_instance_id = Resource::get_attribute(deserialized_resource, "service.instance.id")
  match service_instance_id {
    Some(StringValue(id)) => assert_eq(id, "instance-123")
    _ => assert_true(false)
  }
  
  let service_namespace = Resource::get_attribute(deserialized_resource, "service.namespace")
  match service_namespace {
    Some(StringValue(ns)) => assert_eq(ns, "production")
    _ => assert_true(false)
  }
  
  let service_version = Resource::get_attribute(deserialized_resource, "service.version")
  match service_version {
    Some(StringValue(version)) => assert_eq(version, "2.1.0")
    _ => assert_true(false)
  }
  
  let host_name = Resource::get_attribute(deserialized_resource, "host.name")
  match host_name {
    Some(StringValue(name)) => assert_eq(name, "host-001")
    _ => assert_true(false)
  }
  
  let process_pid = Resource::get_attribute(deserialized_resource, "process.pid")
  match process_pid {
    Some(IntValue(pid)) => assert_eq(pid, 12345)
    _ => assert_true(false)
  }
}

// Test 7: Custom Formatting Functions
test "custom formatting functions" {
  // Create test span
  let span_ctx = SpanContext::new("trace-789", "span-012", true, "custom-state")
  let span = Span::new("test-operation", Client, span_ctx)
  Span::add_event(span, "operation.started", Some([("component", StringValue("test"))]))
  Span::add_event(span, "operation.completed", Some([("component", StringValue("test"))]))
  
  // Format span as human-readable text
  let text_format = Span::format_as_text(span)
  
  // Verify text format contains expected information
  assert_true(text_format.contains("test-operation"))
  assert_true(text_format.contains("Client"))
  assert_true(text_format.contains("trace-789"))
  assert_true(text_format.contains("span-012"))
  assert_true(text_format.contains("operation.started"))
  assert_true(text_format.contains("operation.completed"))
  
  // Format span as compact string
  let compact_format = Span::format_as_compact(span)
  
  // Verify compact format is shorter than text format
  assert_true(compact_format.length() < text_format.length())
  assert_true(compact_format.contains("test-operation"))
  assert_true(compact_format.contains("trace-789"))
  
  // Format span as detailed debug string
  let debug_format = Span::format_as_debug(span)
  
  // Verify debug format contains more detailed information
  assert_true(debug_format.length() > text_format.length())
  assert_true(debug_format.contains("trace-789"))
  assert_true(debug_format.contains("span-012"))
  assert_true(debug_format.contains("custom-state"))
}

// Test 8: Data Validation During Conversion
test "data validation during conversion" {
  // Test invalid string to int conversion
  let invalid_int_string = StringValue("not-a-number")
  let invalid_int_result = AttributeValue::to_int(invalid_int_string)
  match invalid_int_result {
    Some(_) => assert_true(false) // Should not succeed
    None => assert_true(true) // Expected to fail
  }
  
  // Test invalid string to float conversion
  let invalid_float_string = StringValue("also-not-a-number")
  let invalid_float_result = AttributeValue::to_float(invalid_float_string)
  match invalid_float_result {
    Some(_) => assert_true(false) // Should not succeed
    None => assert_true(true) // Expected to fail
  }
  
  // Test invalid string to bool conversion
  let invalid_bool_string = StringValue("maybe")
  let invalid_bool_result = AttributeValue::to_bool(invalid_bool_string)
  match invalid_bool_result {
    Some(_) => assert_true(false) // Should not succeed
    None => assert_true(true) // Expected to fail
  }
  
  // Test out-of-range values
  let large_int_string = StringValue("999999999999999999999999999999")
  let large_int_result = AttributeValue::to_int(large_int_string)
  match large_int_result {
    Some(_) => assert_true(false) // Should not succeed due to overflow
    None => assert_true(true) // Expected to fail
  }
  
  // Test special float values
  let infinity_string = StringValue("infinity")
  let infinity_result = AttributeValue::to_float(infinity_string)
  match infinity_result {
    Some(f) => assert_true(f.is_infinity())
    None => assert_true(false) // Should handle infinity
  }
  
  let nan_string = StringValue("nan")
  let nan_result = AttributeValue::to_float(nan_string)
  match nan_result {
    Some(f) => assert_true(f.is_nan())
    None => assert_true(false) // Should handle NaN
  }
}

// Test 9: Batch Conversion Operations
test "batch conversion operations" {
  // Create array of attribute values
  let values = [
    StringValue("42"),
    StringValue("3.14"),
    StringValue("true"),
    StringValue("test-string"),
    IntValue(100),
    FloatValue(2.718),
    BoolValue(false)
  ]
  
  // Batch convert all to strings
  let string_values = AttributeValue::batch_to_string(values)
  
  // Verify conversions
  assert_eq(string_values.length(), 7)
  assert_eq(string_values[0], Some("42"))
  assert_eq(string_values[1], Some("3.14"))
  assert_eq(string_values[2], Some("true"))
  assert_eq(string_values[3], Some("test-string"))
  assert_eq(string_values[4], Some("100"))
  assert_eq(string_values[5], Some("2.718"))
  assert_eq(string_values[6], Some("false"))
  
  // Batch convert string values to appropriate types
  let string_values_for_conversion = [
    StringValue("123"),
    StringValue("456.789"),
    StringValue("false"),
    StringValue("another-string")
  ]
  
  let converted_values = AttributeValue::batch_auto_convert(string_values_for_conversion)
  
  // Verify auto-conversions
  assert_eq(converted_values.length(), 4)
  match converted_values[0] {
    IntValue(i) => assert_eq(i, 123)
    _ => assert_true(false)
  }
  
  match converted_values[1] {
    FloatValue(f) => assert_eq(f, 456.789)
    _ => assert_true(false)
  }
  
  match converted_values[2] {
    BoolValue(b) => assert_false(b)
    _ => assert_true(false)
  }
  
  match converted_values[3] {
    StringValue(s) => assert_eq(s, "another-string")
    _ => assert_true(false)
  }
}

// Test 10: Format Preserving Transformations
test "format preserving transformations" {
  // Create test data with specific formatting
  let formatted_json = "{\"service\":{\"name\":\"test-service\",\"version\":\"1.0.0\"},\"metrics\":{\"cpu\":75.5,\"memory\":62.3}}"
  
  // Parse and preserve formatting
  let parsed_data = DataFormat::parse_with_formatting(formatted_json)
  
  // Modify data
  let modified_data = DataFormat::set_value(parsed_data, "service.version", StringValue("1.0.1"))
  let modified_data = DataFormat::set_value(modified_data, "metrics.disk", FloatValue(45.8))
  
  // Serialize back preserving original formatting
  let reformatted_json = DataFormat::serialize_with_formatting(modified_data)
  
  // Verify data was updated but formatting preserved
  assert_true(reformatted_json.contains("\"service\":{\"name\":\"test-service\",\"version\":\"1.0.1\""))
  assert_true(reformatted_json.contains("\"metrics\":{\"cpu\":75.5,\"memory\":62.3,\"disk\":45.8}"))
  
  // Test with XML formatting
  let formatted_xml = "<service><name>test-service</name><version>1.0.0</version></service><metrics><cpu>75.5</cpu><memory>62.3</memory></metrics>"
  
  let parsed_xml = DataFormat::parse_xml_with_formatting(formatted_xml)
  let modified_xml = DataFormat::set_xml_value(parsed_xml, "version", "1.0.1")
  let modified_xml = DataFormat::add_xml_element(modified_xml, "disk", "45.8")
  let reformatted_xml = DataFormat::serialize_xml_with_formatting(modified_xml)
  
  // Verify XML was updated but formatting preserved
  assert_true(reformatted_xml.contains("<name>test-service</name>"))
  assert_true(reformatted_xml.contains("<version>1.0.1</version>"))
  assert_true(reformatted_xml.contains("<disk>45.8</disk>"))
}