// Azimuth Core Focused MoonBit Test Suite
// This file contains 10 focused test cases for core telemetry functionality

// Test 1: AttributeValue Type Conversion and Validation
test "attribute value type conversion and validation" {
  let string_val = StringValue("test-value")
  let int_val = IntValue(42)
  let float_val = FloatValue(3.14)
  let bool_val = BoolValue(true)
  let array_string_val = ArrayStringValue(["a", "b", "c"])
  let array_int_val = ArrayIntValue([1, 2, 3])
  
  // Test type matching and extraction
  match string_val {
    StringValue(s) => assert_eq(s, "test-value")
    _ => assert_true(false)
  }
  
  match int_val {
    IntValue(i) => assert_eq(i, 42)
    _ => assert_true(false)
  }
  
  match float_val {
    FloatValue(f) => assert_true(f > 3.0 && f < 3.2)
    _ => assert_true(false)
  }
  
  match bool_val {
    BoolValue(b) => assert_true(b)
    _ => assert_true(false)
  }
  
  match array_string_val {
    ArrayStringValue(arr) => {
      assert_eq(arr.length(), 3)
      assert_true(arr.contains("b"))
    }
    _ => assert_true(false)
  }
  
  match array_int_val {
    ArrayIntValue(arr) => {
      assert_eq(arr.length(), 3)
      assert_eq(arr[0], 1)
    }
    _ => assert_true(false)
  }
}

// Test 2: Resource Attributes Operations
test "resource attributes management and operations" {
  let resource_attrs = [
    ("service.name", StringValue("azimuth-telemetry")),
    ("service.version", StringValue("1.0.0")),
    ("service.instance.id", StringValue("instance-123")),
    ("host.name", StringValue("localhost")),
    ("process.pid", IntValue(1234))
  ]
  
  let resource = { attributes: resource_attrs }
  
  // Test attribute count
  assert_eq(resource.attributes.length(), 5)
  
  // Test finding specific attributes
  let service_name = resource.attributes.find(fn(pair) { pair.0 == "service.name" })
  match service_name {
    Some(pair) => {
      match pair.1 {
        StringValue(name) => assert_eq(name, "azimuth-telemetry")
        _ => assert_true(false)
      }
    }
    None => assert_true(false)
  }
  
  // Test filtering by attribute type
  let string_attrs = resource.attributes.filter(fn(pair) {
    match pair.1 {
      StringValue(_) => true
      _ => false
    }
  })
  assert_eq(string_attrs.length(), 4)
  
  // Test filtering by attribute value pattern
  let service_attrs = resource.attributes.filter(fn(pair) { 
    pair.0.starts_with("service.") 
  })
  assert_eq(service_attrs.length(), 3)
}

// Test 3: Context Key-Value Operations
test "context key-value storage and retrieval" {
  let context_key = { key: "trace-id" }
  let empty_context = { data: None }
  let populated_context = { data: Some(("trace-id", "abc123")) }
  
  // Test empty context
  match empty_context.data {
    Some(_) => assert_true(false)
    None => assert_true(true)
  }
  
  // Test populated context
  match populated_context.data {
    Some((key, value)) => {
      assert_eq(key, "trace-id")
      assert_eq(value, "abc123")
    }
    None => assert_true(false)
  }
  
  // Test context key matching
  let context_key2 = { key: "user-id" }
  assert_not_eq(context_key.key, context_key2.key)
  assert_eq(context_key.key, "trace-id")
}

// Test 4: Baggage Entry Management
test "baggage entry management and propagation" {
  let baggage_entries = [
    ("user-id", "user-123"),
    ("request-id", "req-456"),
    ("session-id", "sess-789")
  ]
  
  let baggage = { entries: baggage_entries }
  
  // Test baggage entry count
  assert_eq(baggage.entries.length(), 3)
  
  // Test finding specific baggage entry
  let user_entry = baggage.entries.find(fn(pair) { pair.0 == "user-id" })
  match user_entry {
    Some(pair) => {
      assert_eq(pair.0, "user-id")
      assert_eq(pair.1, "user-123")
    }
    None => assert_true(false)
  }
  
  // Test baggage entry filtering
  let session_entries = baggage.entries.filter(fn(pair) { 
    pair.0.contains("session") 
  })
  assert_eq(session_entries.length(), 1)
  
  // Test baggage entry transformation
  let uppercased_keys = baggage.entries.map(fn(pair) { 
    (pair.0.to_uppercase(), pair.1) 
  })
  assert_eq(uppercased_keys[0].0, "USER-ID")
}

// Test 5: Span Context Creation and Validation
test "span context creation and trace validation" {
  let span_context = {
    trace_id: "0af7651916cd43dd8448eb211c80319c",
    span_id: "b7ad6b7169203331",
    sampled: true,
    trace_state: "rojo=00f067aa0ba902b7"
  }
  
  // Test trace ID format (should be 32 hex characters)
  assert_eq(span_context.trace_id.length(), 32)
  assert_true(span_context.trace_id.is_hex())
  
  // Test span ID format (should be 16 hex characters)
  assert_eq(span_context.span_id.length(), 16)
  assert_true(span_context.span_id.is_hex())
  
  // Test sampling flag
  assert_true(span_context.sampled)
  
  // Test trace state
  assert_true(span_context.trace_state.length() > 0)
  assert_true(span_context.trace_state.contains("="))
  
  // Test context validation
  assert_true(span_context.trace_id != "")
  assert_true(span_context.span_id != "")
}

// Test 6: TextMap Carrier Header Operations
test "text map carrier header management" {
  let headers = [
    ("traceparent", "00-0af7651916cd43dd8448eb211c80319c-b7ad6b7169203331-01"),
    ("x-correlation-id", "corr-123"),
    ("content-type", "application/json"),
    ("x-request-id", "req-456")
  ]
  
  let carrier = { headers: headers }
  
  // Test header count
  assert_eq(carrier.headers.length(), 4)
  
  // Test finding specific header
  let trace_header = carrier.headers.find(fn(pair) { pair.0 == "traceparent" })
  match trace_header {
    Some(pair) => {
      assert_eq(pair.0, "traceparent")
      assert_true(pair.1.starts_with("00-"))
    }
    None => assert_true(false)
  }
  
  // Test filtering by header prefix
  let x_headers = carrier.headers.filter(fn(pair) { pair.0.starts_with("x-") })
  assert_eq(x_headers.length(), 2)
  
  // Test header case sensitivity
  let lower_headers = carrier.headers.map(fn(pair) { 
    (pair.0.to_lowercase(), pair.1) 
  })
  let trace_lower = lower_headers.find(fn(pair) { pair.0 == "traceparent" })
  assert_true(trace_lower.is_some())
}

// Test 7: Instrumentation Scope Completeness
test "instrumentation scope definition and validation" {
  let complete_scope = {
    name: "azimuth.telemetry",
    version: Some("1.0.0"),
    schema_url: Some("https://opentelemetry.io/schemas/1.20.0")
  }
  
  let minimal_scope = {
    name: "minimal.instrument",
    version: None,
    schema_url: None
  }
  
  // Test complete scope
  assert_eq(complete_scope.name, "azimuth.telemetry")
  match complete_scope.version {
    Some(v) => assert_eq(v, "1.0.0")
    None => assert_true(false)
  }
  match complete_scope.schema_url {
    Some(url) => assert_true(url.starts_with("https://"))
    None => assert_true(false)
  }
  
  // Test minimal scope
  assert_eq(minimal_scope.name, "minimal.instrument")
  assert_true(minimal_scope.version.is_none())
  assert_true(minimal_scope.schema_url.is_none())
  
  // Test scope validation
  assert_true(complete_scope.name.length() > 0)
  assert_true(minimal_scope.name.length() > 0)
}

// Test 8: Attributes Merge Operations
test "attributes merging and conflict resolution" {
  let primary_attrs = [
    ("service.name", StringValue("primary-service")),
    ("service.version", StringValue("1.0.0")),
    ("environment", StringValue("production"))
  ]
  
  let secondary_attrs = [
    ("service.name", StringValue("secondary-service")),  // Conflict
    ("host.name", StringValue("secondary-host")),
    ("region", StringValue("us-west"))
  ]
  
  let primary_attributes = { values: primary_attrs }
  let secondary_attributes = { values: secondary_attrs }
  
  // Test attribute merging (secondary overrides primary for conflicts)
  let merged_values = primary_attributes.values.concat(
    secondary_attributes.values.filter(fn(sec_pair) {
      not(primary_attributes.values.any(fn(pri_pair) { pri_pair.0 == sec_pair.0 }))
    })
  )
  
  // Verify merged results
  assert_eq(merged_values.length(), 4)  // 3 primary + 2 secondary - 1 conflict
  
  // Test that service.name from secondary takes precedence
  let service_name = merged_values.find(fn(pair) { pair.0 == "service.name" })
  match service_name {
    Some(pair) => {
      match pair.1 {
        StringValue(name) => assert_eq(name, "secondary-service")
        _ => assert_true(false)
      }
    }
    None => assert_true(false)
  }
  
  // Test that non-conflicting attributes are preserved
  let environment = merged_values.find(fn(pair) { pair.0 == "environment" })
  assert_true(environment.is_some())
  
  let host_name = merged_values.find(fn(pair) { pair.0 == "host.name" })
  assert_true(host_name.is_some())
}

// Test 9: Telemetry Data Serialization
test "telemetry data serialization integrity" {
  let test_attributes = [
    ("string.attr", StringValue("test-value")),
    ("int.attr", IntValue(123)),
    ("float.attr", FloatValue(45.67)),
    ("bool.attr", BoolValue(true))
  ]
  
  let test_resource = { attributes: test_attributes }
  
  // Test attribute serialization to string representation
  let serialized_attrs = test_attributes.map(fn(pair) {
    let value_str = match pair.1 {
      StringValue(s) => "string:" + s
      IntValue(i) => "int:" + i.to_string()
      FloatValue(f) => "float:" + f.to_string()
      BoolValue(b) => "bool:" + (if b { "true" } else { "false" })
      _ => "unknown"
    }
    pair.0 + "=" + value_str
  })
  
  // Verify serialization
  assert_eq(serialized_attrs.length(), 4)
  assert_true(serialized_attrs.contains("string.attr=string:test-value"))
  assert_true(serialized_attrs.contains("int.attr=int:123"))
  assert_true(serialized_attrs.contains("float.attr=float:45.67"))
  assert_true(serialized_attrs.contains("bool.attr=bool:true"))
  
  // Test deserialization back to attributes
  let deserialized = serialized_attrs.map(fn(serialized) {
    let parts = serialized.split("=")
    if parts.length() == 2 {
      let key = parts[0]
      let value_parts = parts[1].split(":")
      if value_parts.length() == 2 {
        let value = match value_parts[0] {
          "string" => StringValue(value_parts[1])
          "int" => IntValue(value_parts[1].to_int())
          "float" => FloatValue(value_parts[1].to_float())
          "bool" => BoolValue(value_parts[1] == "true")
          _ => StringValue("unknown")
        }
        (key, value)
      } else {
        (key, StringValue("invalid"))
      }
    } else {
      ("invalid", StringValue("invalid"))
    }
  })
  
  assert_eq(deserialized.length(), 4)
}

// Test 10: Cross-Service Propagation Basic Functionality
test "cross-service propagation basic functionality" {
  // Simulate propagation headers
  let propagation_headers = [
    ("traceparent", "00-0af7651916cd43dd8448eb211c80319c-b7ad6b7169203331-01"),
    ("tracestate", "rojo=00f067aa0ba902b7,congo=t61rcWkgMzE"),
    ("baggage", "userId=user123,sessionId=session456"),
    ("x-correlation-id", "corr-789")
  ]
  
  let carrier = { headers: propagation_headers }
  
  // Test traceparent extraction
  let traceparent = carrier.headers.find(fn(pair) { pair.0 == "traceparent" })
  match traceparent {
    Some(pair) => {
      let parts = pair.1.split("-")
      assert_eq(parts.length(), 4)
      assert_eq(parts[0], "00")  // Version
      assert_eq(parts[3], "01")  // Flags
    }
    None => assert_true(false)
  }
  
  // Test tracestate parsing
  let tracestate = carrier.headers.find(fn(pair) { pair.0 == "tracestate" })
  match tracestate {
    Some(pair) => {
      let entries = pair.1.split(",")
      assert_eq(entries.length(), 2)
      assert_true(entries[0].contains("rojo="))
      assert_true(entries[1].contains("congo="))
    }
    None => assert_true(false)
  }
  
  // Test baggage parsing
  let baggage = carrier.headers.find(fn(pair) { pair.0 == "baggage" })
  match baggage {
    Some(pair) => {
      let entries = pair.1.split(",")
      assert_eq(entries.length(), 2)
      assert_true(entries[0].contains("userId="))
      assert_true(entries[1].contains("sessionId="))
    }
    None => assert_true(false)
  }
  
  // Test correlation ID preservation
  let correlation_id = carrier.headers.find(fn(pair) { pair.0 == "x-correlation-id" })
  match correlation_id {
    Some(pair) => assert_eq(pair.1, "corr-789")
    None => assert_true(false)
  }
}