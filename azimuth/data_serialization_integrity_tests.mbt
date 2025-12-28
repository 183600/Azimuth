// Data Serialization and Integrity Tests for Azimuth Telemetry System
// Focus on data serialization, deserialization, and integrity verification

test "attribute_value_serialization_consistency" {
  // Test serialization consistency of different attribute value types
  let string_val = StringValue("test_string")
  let int_val = IntValue(42)
  let float_val = FloatValue(3.14159)
  let bool_val = BoolValue(true)
  let array_string_val = ArrayStringValue(["item1", "item2", "item3"])
  let array_int_val = ArrayIntValue([1, 2, 3, 4, 5])
  
  // Simulate serialization and deserialization
  let serialized_string = match string_val {
    StringValue(s) => "string:" + s
    _ => "unknown"
  }
  
  let serialized_int = match int_val {
    IntValue(i) => "int:" + i.to_string()
    _ => "unknown"
  }
  
  let serialized_float = match float_val {
    FloatValue(f) => "float:" + f.to_string()
    _ => "unknown"
  }
  
  let serialized_bool = match bool_val {
    BoolValue(b) => "bool:" + b.to_string()
    _ => "unknown"
  }
  
  // Verify serialization format consistency
  @assertion.assert_eq(serialized_string, "string:test_string")?
  @assertion.assert_eq(serialized_int, "int:42")?
  @assertion.assert_eq(serialized_float, "float:3.14159")?
  @assertion.assert_eq(serialized_bool, "bool:true")?
  
  // Test deserialization back to original values
  let deserialized_string = match serialized_string.split(":") {
    [type_str, value_str] if type_str == "string" => StringValue(value_str)
    _ => StringValue("")
  }
  
  let deserialized_int = match serialized_int.split(":") {
    [type_str, value_str] if type_str == "int" => IntValue(value_str.to_int())
    _ => IntValue(0)
  }
  
  @assertion.assert_eq(deserialized_string, string_val)?
  @assertion.assert_eq(deserialized_int, int_val)?
}

test "span_context_data_integrity" {
  // Test span context data integrity through serialization
  let original_ctx = SpanContext::new("trace123456", "span789012", true, "key1=value1&key2=value2")
  
  // Simulate serialization
  let serialized = original_ctx.trace_id + "|" + original_ctx.span_id + "|" + 
                  original_ctx.sampled.to_string() + "|" + original_ctx.trace_state
  
  // Simulate deserialization
  let parts = serialized.split("|")
  let reconstructed_ctx = match parts {
    [trace_id, span_id, sampled_str, trace_state] => {
      SpanContext::new(trace_id, span_id, sampled_str == "true", trace_state)
    }
    _ => SpanContext::new("", "", false, "")
  }
  
  // Verify data integrity
  @assertion.assert_eq(reconstructed_ctx.trace_id, original_ctx.trace_id)?
  @assertion.assert_eq(reconstructed_ctx.span_id, original_ctx.span_id)?
  @assertion.assert_eq(reconstructed_ctx.sampled, original_ctx.sampled)?
  @assertion.assert_eq(reconstructed_ctx.trace_state, original_ctx.trace_state)?
  @assertion.assert_eq(SpanContext::is_valid(reconstructed_ctx), SpanContext::is_valid(original_ctx))?
}

test "context_serialization_with_special_characters" {
  let ctx = Context::root()
  let special_key = ContextKey::new("special.key")
  let special_value = "value with spaces & special chars: !@#$%^&*()_+{}|:<>?[]\\;'",./<>"
  
  let ctx_with_special = Context::with_value(ctx, special_key, special_value)
  
  // Simulate serialization with URL encoding
  let serialized_value = special_value.replace(" ", "%20")
                              .replace("&", "%26")
                              .replace("=", "%3D")
                              .replace("?", "%3F")
  
  // Simulate deserialization with URL decoding
  let deserialized_value = serialized_value.replace("%20", " ")
                                .replace("%26", "&")
                                .replace("%3D", "=")
                                .replace("%3F", "?")
  
  // Verify integrity
  @assertion.assert_eq(Context::get(ctx_with_special, special_key), Some(special_value))?
  @assertion.assert_eq(deserialized_value, special_value)?
}

test "log_record_comprehensive_serialization" {
  let attrs = Attributes::new()
  Attributes::set(attrs, "error.code", IntValue(500))
  Attributes::set(attrs, "error.message", StringValue("Internal server error"))
  
  let log = LogRecord::new_with_context(
    Error,
    Some("Database connection failed"),
    Some(attrs),
    Some(1735689600000000000L),
    Some(1735689600000001000L),
    Some("trace123456"),
    Some("span789012"),
    None
  )
  
  // Simulate comprehensive serialization
  let serialized = "severity:" + log.severity.to_string() + "|" +
                  "body:" + log.body.or_else("") + "|" +
                  "timestamp:" + log.timestamp.or_else("0") + "|" +
                  "trace_id:" + log.trace_id.or_else("") + "|" +
                  "span_id:" + log.span_id.or_else("")
  
  // Verify serialization contains all expected data
  @assertion.assert_true(serialized.contains("severity:Error"))?
  @assertion.assert_true(serialized.contains("body:Database connection failed"))?
  @assertion.assert_true(serialized.contains("timestamp:1735689600000000000"))?
  @assertion.assert_true(serialized.contains("trace_id:trace123456"))?
  @assertion.assert_true(serialized.contains("span_id:span789012"))?
}

test "resource_attributes_serialization_order" {
  let resource = Resource::new()
  let attributes = [
    ("service.name", StringValue("my-service")),
    ("service.version", StringValue("1.0.0")),
    ("service.instance.id", StringValue("instance-123")),
    ("environment", StringValue("production")),
    ("region", StringValue("us-west-2"))
  ]
  
  let resource_with_attrs = Resource::with_attributes(resource, attributes)
  
  // Simulate ordered serialization
  let serialized_parts = []
  for attr in attributes {
    let serialized = attr.0 + "=" + match attr.1 {
      StringValue(s) => s
      IntValue(i) => i.to_string()
      FloatValue(f) => f.to_string()
      BoolValue(b) => b.to_string()
      ArrayStringValue(arr) => "[" + arr.join(",") + "]"
      ArrayIntValue(arr) => "[" + arr.map(fn(x) { x.to_string() }).join(",") + "]"
    }
    serialized_parts.push(serialized)
  }
  
  let serialized = serialized_parts.join("&")
  
  // Verify all attributes are present in serialization
  @assertion.assert_true(serialized.contains("service.name=my-service"))?
  @assertion.assert_true(serialized.contains("service.version=1.0.0"))?
  @assertion.assert_true(serialized.contains("service.instance.id=instance-123"))?
  @assertion.assert_true(serialized.contains("environment=production"))?
  @assertion.assert_true(serialized.contains("region=us-west-2"))?
}

test "baggage_serialization_with_commas" {
  let baggage = Baggage::new()
  
  // Add baggage entries that need proper comma escaping
  let baggage1 = Baggage::set_entry(baggage, "user.id", "12345")
  let baggage2 = Baggage::set_entry(baggage1, "user.name", "John,Doe")  // Contains comma
  let baggage3 = Baggage::set_entry(baggage2, "session.data", "value1,value2,value3")  // Multiple commas
  
  // Simulate W3C baggage serialization format
  let serialized_entries = []
  let entries = [
    ("user.id", "12345"),
    ("user.name", "John,Doe"),
    ("session.data", "value1,value2,value3")
  ]
  
  for entry in entries {
    let serialized = entry.0 + "=" + entry.1
    serialized_entries.push(serialized)
  }
  
  let serialized = serialized_entries.join(",")
  
  // Verify serialization format
  @assertion.assert_true(serialized.contains("user.id=12345"))?
  @assertion.assert_true(serialized.contains("user.name=John,Doe"))?
  @assertion.assert_true(serialized.contains("session.data=value1,value2,value3"))?
  
  // Verify proper comma separation
  let parts = serialized.split(",")
  @assertion.assert_eq(parts.length(), 3)?
}

test "http_headers_serialization_integrity" {
  let headers = [
    ("Content-Type", "application/json"),
    ("Authorization", "Bearer token123"),
    ("X-Custom-Header", "custom:value:with:colons"),
    ("X-Array-Header", "value1,value2,value3"),
    ("X-Empty-Header", "")
  ]
  
  // Simulate HTTP header serialization
  let serialized_headers = []
  for header in headers {
    let serialized = header.0 + ": " + header.1
    serialized_headers.push(serialized)
  }
  
  let serialized = serialized_headers.join("\r\n")
  
  // Verify serialization integrity
  @assertion.assert_true(serialized.contains("Content-Type: application/json"))?
  @assertion.assert_true(serialized.contains("Authorization: Bearer token123"))?
  @assertion.assert_true(serialized.contains("X-Custom-Header: custom:value:with:colons"))?
  @assertion.assert_true(serialized.contains("X-Array-Header: value1,value2,value3"))?
  @assertion.assert_true(serialized.contains("X-Empty-Header: "))?
  
  // Test deserialization back to headers
  let lines = serialized.split("\r\n")
  let deserialized_headers = []
  for line in lines {
    let parts = line.split(": ", 2)
    match parts {
      [key, value] => deserialized_headers.push((key, value))
      _ => ()
    }
  }
  
  @assertion.assert_eq(deserialized_headers.length(), 5)?
}

test "traceparent_header_format_validation" {
  // Test W3C traceparent header format: 00-trace-id-span-id-flags
  let valid_traceparent = "00-0af7651916cd43dd8448eb211c80319c-b7ad6b7169203331-01"
  let invalid_traceparent = "invalid-format"
  let incomplete_traceparent = "00-traceid"
  
  // Validate traceparent format
  let validate_traceparent = fn(header : String) -> Bool {
    let parts = header.split("-")
    match parts {
      ["00", trace_id, span_id, flags] => {
        trace_id.length() == 32 && span_id.length() == 16 && flags.length() == 2
      }
      _ => false
    }
  }
  
  @assertion.assert_eq(validate_traceparent(valid_traceparent), true)?
  @assertion.assert_eq(validate_traceparent(invalid_traceparent), false)?
  @assertion.assert_eq(validate_traceparent(incomplete_traceparent), false)?
  
  // Test extraction from traceparent
  let extract_trace_id = fn(header : String) -> String {
    let parts = header.split("-")
    match parts {
      ["00", trace_id, _, _] => trace_id
      _ => ""
    }
  }
  
  let extract_span_id = fn(header : String) -> String {
    let parts = header.split("-")
    match parts {
      ["00", _, span_id, _] => span_id
      _ => ""
    }
  }
  
  @assertion.assert_eq(extract_trace_id(valid_traceparent), "0af7651916cd43dd8448eb211c80319c")?
  @assertion.assert_eq(extract_span_id(valid_traceparent), "b7ad6b7169203331")?
}

test "data_corruption_detection" {
  // Test detection of data corruption through checksums
  let original_data = "important telemetry data"
  
  // Simulate simple checksum calculation
  let calculate_checksum = fn(data : String) -> Int {
    let sum = 0
    for char in data.to_array() {
      sum = sum + char.to_int()
    }
    sum % 10000  // Simple modulo to keep it manageable
  }
  
  let original_checksum = calculate_checksum(original_data)
  
  // Simulate data corruption
  let corrupted_data = "important telemetry d@ta"  // Changed one character
  let corrupted_checksum = calculate_checksum(corrupted_data)
  
  // Verify checksums are different
  @assertion.assert_true(original_checksum != corrupted_checksum)?
  
  // Test data integrity verification
  let verify_integrity = fn(data : String, expected_checksum : Int) -> Bool {
    calculate_checksum(data) == expected_checksum
  }
  
  @assertion.assert_eq(verify_integrity(original_data, original_checksum), true)?
  @assertion.assert_eq(verify_integrity(corrupted_data, original_checksum), false)?
}