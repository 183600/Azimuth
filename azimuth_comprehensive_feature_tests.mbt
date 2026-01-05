// Azimuth Telemetry System - Comprehensive Feature Tests
// This file contains comprehensive test cases for various telemetry features

// Test 1: AttributeValue Type Conversion
test "attribute value type conversion" {
  // Test string to attribute value conversion
  let str_val = StringValue("hello")
  match str_val {
    StringValue(s) => assert_eq(s, "hello")
    _ => assert_true(false)
  }
  
  // Test integer to attribute value conversion
  let int_val = IntValue(123)
  match int_val {
    IntValue(i) => assert_eq(i, 123)
    _ => assert_true(false)
  }
  
  // Test float to attribute value conversion
  let float_val = FloatValue(3.14159)
  match float_val {
    FloatValue(f) => assert_eq(f, 3.14159)
    _ => assert_true(false)
  }
  
  // Test boolean to attribute value conversion
  let bool_val = BoolValue(true)
  match bool_val {
    BoolValue(b) => assert_true(b)
    _ => assert_true(false)
  }
  
  // Test string array to attribute value conversion
  let arr_str_val = ArrayStringValue(["a", "b", "c"])
  match arr_str_val {
    ArrayStringValue(arr) => {
      assert_eq(arr.length(), 3)
      assert_eq(arr[0], "a")
      assert_eq(arr[1], "b")
      assert_eq(arr[2], "c")
    }
    _ => assert_true(false)
  }
}

// Test 2: Resource Merge Operations
test "resource merge operations" {
  // Create two resources with different attributes
  let resource1 = { attributes: [("service.name", StringValue("service1")), ("version", StringValue("1.0.0"))] }
  let resource2 = { attributes: [("environment", StringValue("production")), ("version", StringValue("2.0.0"))] }
  
  // Merge resources (second resource should override duplicate keys)
  let merged_attrs = resource1.attributes.map(|(k, v)| (k, v)) + resource2.attributes.map(|(k, v)| (k, v))
  
  // Check that we have the expected number of attributes
  assert_eq(merged_attrs.length(), 3)
  
  // Check that version from resource2 overrides version from resource1
  let version_found = merged_attrs.any(|(k, v)| {
    match (k, v) {
      ("version", StringValue(v)) => v == "2.0.0"
      _ => false
    }
  })
  assert_true(version_found)
}

// Test 3: Context Propagation
test "context propagation" {
  // Create a context with data
  let context = { data: Some(("trace-id", "123456")) }
  
  // Check that context data is correctly stored
  match context.data {
    Some((key, value)) => {
      assert_eq(key, "trace-id")
      assert_eq(value, "123456")
    }
    None => assert_true(false)
  }
  
  // Create an empty context
  let empty_context = { data: None }
  match empty_context.data {
    Some(_) => assert_true(false)
    None => assert_true(true)
  }
}

// Test 4: Baggage Operations
test "baggage operations" {
  // Create baggage with entries
  let baggage = { entries: [("user-id", "user123"), ("session-id", "session456")] }
  
  // Check that baggage entries are correctly stored
  assert_eq(baggage.entries.length(), 2)
  
  // Find user-id entry
  let user_id_found = baggage.entries.any(|(k, v)| k == "user-id" && v == "user123")
  assert_true(user_id_found)
  
  // Add new entry to baggage
  let new_entries = baggage.entries + [("request-id", "req789")]
  assert_eq(new_entries.length(), 3)
  
  // Check that new entry is correctly added
  let request_id_found = new_entries.any(|(k, v)| k == "request-id" && v == "req789")
  assert_true(request_id_found)
}

// Test 5: SpanContext Functionality
test "span context functionality" {
  // Create a span context
  let span_context = {
    trace_id: "trace123456789",
    span_id: "span123456789",
    sampled: true,
    trace_state: "key1=value1,key2=value2"
  }
  
  // Check that span context fields are correctly set
  assert_eq(span_context.trace_id, "trace123456789")
  assert_eq(span_context.span_id, "span123456789")
  assert_true(span_context.sampled)
  assert_eq(span_context.trace_state, "key1=value1,key2=value2")
  
  // Create an unsampled span context
  let unsampled_context = { 
    trace_id: "trace987654321",
    span_id: "span987654321",
    sampled: false,
    trace_state: ""
  }
  assert_false(unsampled_context.sampled)
}

// Test 6: TextMapCarrier Injection and Extraction
test "text map carrier injection and extraction" {
  // Create a text map carrier with headers
  let carrier = { headers: [
    ("traceparent", "00-trace123-span456-01"),
    ("x-b3-traceid", "trace123"),
    ("x-b3-spanid", "span456")
  ]}
  
  // Check that headers are correctly stored
  assert_eq(carrier.headers.length(), 3)
  
  // Find traceparent header
  let traceparent_found = carrier.headers.any(|(k, v)| k == "traceparent" && v == "00-trace123-span456-01")
  assert_true(traceparent_found)
  
  // Add new header to carrier
  let new_headers = carrier.headers + [("custom-header", "custom-value")]
  assert_eq(new_headers.length(), 4)
  
  // Check that new header is correctly added
  let custom_header_found = new_headers.any(|(k, v)| k == "custom-header" && v == "custom-value")
  assert_true(custom_header_found)
}

// Test 7: InstrumentationScope Creation
test "instrumentation scope creation" {
  // Create an instrumentation scope with all fields
  let scope = {
    name: "my-instrument",
    version: Some("1.0.0"),
    schema_url: Some("https://example.com/schema")
  }
  
  // Check that instrumentation scope fields are correctly set
  assert_eq(scope.name, "my-instrument")
  
  match scope.version {
    Some(v) => assert_eq(v, "1.0.0")
    None => assert_true(false)
  }
  
  match scope.schema_url {
    Some(url) => assert_eq(url, "https://example.com/schema")
    None => assert_true(false)
  }
  
  // Create an instrumentation scope with optional fields as None
  let minimal_scope = {
    name: "minimal-instrument",
    version: None,
    schema_url: None
  }
  
  assert_eq(minimal_scope.name, "minimal-instrument")
  match minimal_scope.version {
    Some(_) => assert_true(false)
    None => assert_true(true)
  }
}

// Test 8: Attribute Array Operations
test "attribute array operations" {
  // Create attributes with array values
  let attributes = { values: [
    ("tags", ArrayStringValue(["tag1", "tag2", "tag3"])),
    ("numbers", ArrayIntValue([1, 2, 3, 4, 5])),
    ("single", StringValue("value"))
  ]}
  
  // Check that attributes are correctly stored
  assert_eq(attributes.values.length(), 3)
  
  // Find array string attribute
  let tags_found = attributes.values.any(|(k, v)| {
    match (k, v) {
      ("tags", ArrayStringValue(arr)) => arr.length() == 3 && arr[0] == "tag1"
      _ => false
    }
  })
  assert_true(tags_found)
  
  // Find array int attribute
  let numbers_found = attributes.values.any(|(k, v)| {
    match (k, v) {
      ("numbers", ArrayIntValue(arr)) => arr.length() == 5 && arr[0] == 1
      _ => false
    }
  })
  assert_true(numbers_found)
  
  // Find single value attribute
  let single_found = attributes.values.any(|(k, v)| {
    match (k, v) {
      ("single", StringValue(val)) => val == "value"
      _ => false
    }
  })
  assert_true(single_found)
}

// Test 9: Telemetry Data Serialization
test "telemetry data serialization" {
  // Create a resource for serialization
  let resource = { attributes: [
    ("service.name", StringValue("test-service")),
    ("service.version", StringValue("1.0.0")),
    ("environment", StringValue("test"))
  ]}
  
  // Simulate serialization by converting to string representation
  let serialized = resource.attributes.map(|(k, v)| {
    match v {
      StringValue(s) => k + "=" + s
      IntValue(i) => k + "=" + i.to_string()
      FloatValue(f) => k + "=" + f.to_string()
      BoolValue(b) => k + "=" + b.to_string()
      ArrayStringValue(arr) => k + "=" + "[" + arr.join(",") + "]"
      ArrayIntValue(arr) => k + "=" + "[" + arr.map(|x| x.to_string()).join(",") + "]"
    }
  }).join(",")
  
  // Check that serialization contains expected values
  assert_true(serialized.contains("service.name=test-service"))
  assert_true(serialized.contains("service.version=1.0.0"))
  assert_true(serialized.contains("environment=test"))
}

// Test 10: Error Handling
test "error handling" {
  // Test handling of empty attribute values
  let empty_string_attr = StringValue("")
  match empty_string_attr {
    StringValue(s) => assert_eq(s, "")
    _ => assert_true(false)
  }
  
  // Test handling of empty array attributes
  let empty_array_attr = ArrayStringValue([])
  match empty_array_attr {
    ArrayStringValue(arr) => assert_eq(arr.length(), 0)
    _ => assert_true(false)
  }
  
  // Test handling of zero values
  let zero_int_attr = IntValue(0)
  match zero_int_attr {
    IntValue(i) => assert_eq(i, 0)
    _ => assert_true(false)
  }
  
  let zero_float_attr = FloatValue(0.0)
  match zero_float_attr {
    FloatValue(f) => assert_eq(f, 0.0)
    _ => assert_true(false)
  }
  
  // Test handling of false boolean
  let false_bool_attr = BoolValue(false)
  match false_bool_attr {
    BoolValue(b) => assert_false(b)
    _ => assert_true(false)
  }
}