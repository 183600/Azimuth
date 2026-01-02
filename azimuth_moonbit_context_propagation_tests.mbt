// Azimuth Telemetry System - MoonBit Context Propagation Tests
// This file contains test cases for context propagation mechanisms

// Test 1: Basic context creation and storage
test "basic context creation and storage" {
  // Create context with data
  let context = Context({ data = Some("user.id", "12345") })
  
  match context.data {
    Some((key, value)) => {
      assert_eq(key, "user.id")
      assert_eq(value, "12345")
    }
    None => assert_true(false)
  }
  
  // Create empty context
  let empty_context = Context({ data = None })
  match empty_context.data {
    Some(_) => assert_true(false)
    None => assert_true(true)
  }
}

// Test 2: Context key operations
test "context key operations" {
  let string_key = ContextKey[String]({ key = "user.context" })
  let int_key = ContextKey[Int]({ key = "request.count" })
  let bool_key = ContextKey[Bool]({ key = "feature.enabled" })
  
  assert_eq(string_key.key, "user.context")
  assert_eq(int_key.key, "request.count")
  assert_eq(bool_key.key, "feature.enabled")
  
  // Test key uniqueness
  assert_true(string_key.key != int_key.key)
  assert_true(int_key.key != bool_key.key)
  assert_true(string_key.key != bool_key.key)
}

// Test 3: Baggage operations and propagation
test "baggage operations and propagation" {
  // Create baggage with multiple entries
  let baggage = Baggage({ entries = [
    ("user.id", "12345"),
    ("request.id", "req-67890"),
    ("trace.origin", "frontend"),
    ("service.version", "1.0.0")
  ] })
  
  assert_eq(baggage.entries.length(), 4)
  
  // Verify all entries
  let mut found_user_id = false
  let mut found_request_id = false
  let mut found_trace_origin = false
  let mut found_service_version = false
  
  for (key, value) in baggage.entries {
    match key {
      "user.id" => {
        assert_eq(value, "12345")
        found_user_id = true
      }
      "request.id" => {
        assert_eq(value, "req-67890")
        found_request_id = true
      }
      "trace.origin" => {
        assert_eq(value, "frontend")
        found_trace_origin = true
      }
      "service.version" => {
        assert_eq(value, "1.0.0")
        found_service_version = true
      }
      _ => assert_true(false)  // Unexpected key
    }
  }
  
  assert_true(found_user_id)
  assert_true(found_request_id)
  assert_true(found_trace_origin)
  assert_true(found_service_version)
}

// Test 4: Baggage merging and inheritance
test "baggage merging and inheritance" {
  let parent_baggage = Baggage({ entries = [
    ("user.id", "12345"),
    ("trace.origin", "frontend")
  ] })
  
  let child_baggage = Baggage({ entries = [
    ("request.id", "req-67890"),
    ("operation.name", "process_data")
  ] })
  
  // Simulate baggage inheritance (parent + child)
  let mut inherited_entries = parent_baggage.entries
  
  // Add child entries
  for (key, value) in child_baggage.entries {
    inherited_entries = inherited_entries @ [(key, value)]
  }
  
  let inherited_baggage = Baggage({ entries = inherited_entries })
  
  // Should have all entries
  assert_eq(inherited_baggage.entries.length(), 4)
  
  // Verify parent entries are preserved
  let mut found_user_id = false
  let mut found_trace_origin = false
  
  // Verify child entries are added
  let mut found_request_id = false
  let mut found_operation_name = false
  
  for (key, value) in inherited_baggage.entries {
    match key {
      "user.id" => {
        assert_eq(value, "12345")
        found_user_id = true
      }
      "trace.origin" => {
        assert_eq(value, "frontend")
        found_trace_origin = true
      }
      "request.id" => {
        assert_eq(value, "req-67890")
        found_request_id = true
      }
      "operation.name" => {
        assert_eq(value, "process_data")
        found_operation_name = true
      }
      _ => assert_true(false)  // Unexpected key
    }
  }
  
  assert_true(found_user_id)
  assert_true(found_trace_origin)
  assert_true(found_request_id)
  assert_true(found_operation_name)
}

// Test 5: Span context propagation
test "span context propagation" {
  // Create parent span context
  let parent_span_context = SpanContext({
    trace_id = "trace-123456789",
    span_id = "span-111111111",
    sampled = true,
    trace_state = "key1=value1,key2=value2"
  })
  
  // Create child span context (inherits from parent)
  let child_span_context = SpanContext({
    trace_id = parent_span_context.trace_id,  // Same trace ID
    span_id = "span-222222222",  // Different span ID
    sampled = parent_span_context.sampled,  // Inherit sampling decision
    trace_state = parent_span_context.trace_state  // Inherit trace state
  })
  
  // Verify inheritance
  assert_eq(child_span_context.trace_id, parent_span_context.trace_id)
  assert_eq(child_span_context.trace_id, "trace-123456789")
  assert_eq(child_span_context.span_id, "span-222222222")
  assert_eq(child_span_context.sampled, parent_span_context.sampled)
  assert_true(child_span_context.sampled)
  assert_eq(child_span_context.trace_state, parent_span_context.trace_state)
  assert_eq(child_span_context.trace_state, "key1=value1,key2=value2")
  
  // Verify different span IDs
  assert_true(parent_span_context.span_id != child_span_context.span_id)
}

// Test 6: TextMapCarrier injection and extraction
test "text map carrier injection and extraction" {
  // Create carrier with trace context
  let carrier = TextMapCarrier({ headers = [
    ("traceparent", "00-0af7651916cd43dd8448eb211c80319c-b7ad6b7169203331-01"),
    ("tracestate", "rojo=00f067aa0ba902b7,congo=t61rcWkgMzE"),
    ("baggage", "key1=value1,key2=value2"),
    ("x-correlation-id", "correlation-12345")
  ] })
  
  assert_eq(carrier.headers.length(), 4)
  
  // Extract traceparent
  let mut traceparent_found = false
  let mut tracestate_found = false
  let mut baggage_found = false
  let mut correlation_id_found = false
  
  for (key, value) in carrier.headers {
    match key {
      "traceparent" => {
        assert_eq(value, "00-0af7651916cd43dd8448eb211c80319c-b7ad6b7169203331-01")
        traceparent_found = true
      }
      "tracestate" => {
        assert_eq(value, "rojo=00f067aa0ba902b7,congo=t61rcWkgMzE")
        tracestate_found = true
      }
      "baggage" => {
        assert_eq(value, "key1=value1,key2=value2")
        baggage_found = true
      }
      "x-correlation-id" => {
        assert_eq(value, "correlation-12345")
        correlation_id_found = true
      }
      _ => assert_true(false)  // Unexpected key
    }
  }
  
  assert_true(traceparent_found)
  assert_true(tracestate_found)
  assert_true(baggage_found)
  assert_true(correlation_id_found)
}

// Test 7: Context propagation through multiple layers
test "context propagation through multiple layers" {
  // Layer 1: Initial context
  let initial_context = Context({ data = Some("user.id", "12345") })
  
  // Layer 2: Service A adds request context
  let service_a_context = Context({ data = Some("request.id", "req-67890") })
  
  // Layer 3: Service B adds operation context
  let service_b_context = Context({ data = Some("operation.name", "process_data") })
  
  // Layer 4: Service C adds timing context
  let service_c_context = Context({ data = Some("start.time", "2023-01-01T00:00:00Z") })
  
  // Collect all context data
  let mut all_context_data = []
  
  match initial_context.data {
    Some((key, value)) => all_context_data = all_context_data @ [(key, value)]
    None => ()
  }
  
  match service_a_context.data {
    Some((key, value)) => all_context_data = all_context_data @ [(key, value)]
    None => ()
  }
  
  match service_b_context.data {
    Some((key, value)) => all_context_data = all_context_data @ [(key, value)]
    None => ()
  }
  
  match service_c_context.data {
    Some((key, value)) => all_context_data = all_context_data @ [(key, value)]
    None => ()
  }
  
  assert_eq(all_context_data.length(), 4)
  
  // Verify all context is preserved
  let mut found_user_id = false
  let mut found_request_id = false
  let mut found_operation_name = false
  let mut found_start_time = false
  
  for (key, value) in all_context_data {
    match key {
      "user.id" => {
        assert_eq(value, "12345")
        found_user_id = true
      }
      "request.id" => {
        assert_eq(value, "req-67890")
        found_request_id = true
      }
      "operation.name" => {
        assert_eq(value, "process_data")
        found_operation_name = true
      }
      "start.time" => {
        assert_eq(value, "2023-01-01T00:00:00Z")
        found_start_time = true
      }
      _ => assert_true(false)  // Unexpected key
    }
  }
  
  assert_true(found_user_id)
  assert_true(found_request_id)
  assert_true(found_operation_name)
  assert_true(found_start_time)
}

// Test 8: Baggage serialization and deserialization
test "baggage serialization and deserialization" {
  let baggage = Baggage({ entries = [
    ("user.id", "12345"),
    ("request.id", "req-67890"),
    ("operation.name", "process_data")
  ] })
  
  // Serialize baggage to string format
  let mut serialized = ""
  let mut first = true
  for (key, value) in baggage.entries {
    if first {
      serialized = serialized + key + "=" + value
      first = false
    } else {
      serialized = serialized + "," + key + "=" + value
    }
  }
  
  assert_eq(serialized, "user.id=12345,request.id=req-67890,operation.name=process_data")
  
  // Deserialize baggage from string
  let mut deserialized_entries = []
  let parts = serialized.split(",")
  
  for part in parts {
    let key_value = part.split("=")
    if key_value.length() == 2 {
      let key = key_value[0]
      let value = key_value[1]
      deserialized_entries = deserialized_entries @ [(key, value)]
    }
  }
  
  let deserialized_baggage = Baggage({ entries = deserialized_entries })
  
  // Verify deserialization
  assert_eq(deserialized_baggage.entries.length(), 3)
  
  for (key, value) in deserialized_baggage.entries {
    match key {
      "user.id" => assert_eq(value, "12345")
      "request.id" => assert_eq(value, "req-67890")
      "operation.name" => assert_eq(value, "process_data")
      _ => assert_true(false)  // Unexpected key
    }
  }
}

// Test 9: Context propagation with empty values
test "context propagation with empty values" {
  // Test with empty baggage
  let empty_baggage = Baggage({ entries = [] })
  assert_eq(empty_baggage.entries.length(), 0)
  
  // Test with empty string values
  let empty_string_baggage = Baggage({ entries = [
    ("empty.string", ""),
    ("normal.string", "value")
  ] })
  
  assert_eq(empty_string_baggage.entries.length(), 2)
  
  let mut found_empty_string = false
  let mut found_normal_string = false
  
  for (key, value) in empty_string_baggage.entries {
    match key {
      "empty.string" => {
        assert_eq(value, "")
        found_empty_string = true
      }
      "normal.string" => {
        assert_eq(value, "value")
        found_normal_string = true
      }
      _ => assert_true(false)  // Unexpected key
    }
  }
  
  assert_true(found_empty_string)
  assert_true(found_normal_string)
  
  // Test with empty context
  let empty_context = Context({ data = None })
  match empty_context.data {
    Some(_) => assert_true(false)
    None => assert_true(true)
  }
  
  // Test with empty trace state
  let empty_trace_state_context = SpanContext({
    trace_id = "trace-123456789",
    span_id = "span-111111111",
    sampled = true,
    trace_state = ""
  })
  
  assert_eq(empty_trace_state_context.trace_state, "")
}

// Test 10: Context propagation with special characters
test "context propagation with special characters" {
  // Test baggage with special characters
  let special_baggage = Baggage({ entries = [
    ("key.with.dots", "value.with.dots"),
    ("key-with-dashes", "value-with-dashes"),
    ("key_with_underscores", "value_with_underscores"),
    ("key/with/slashes", "value/with/slashes"),
    ("key with spaces", "value with spaces"),
    ("key.with.special.chars!@#$%", "value.with.special.chars!@#$%")
  ] })
  
  assert_eq(special_baggage.entries.length(), 6)
  
  // Verify all special characters are preserved
  for (key, value) in special_baggage.entries {
    // Key and value should have the same structure
    assert_eq(key.replace(".", " ").replace("-", " ").replace("_", " ").replace("/", " "),
             value.replace(".", " ").replace("-", " ").replace("_", " ").replace("/", " "))
  }
  
  // Test trace state with special characters
  let special_trace_state = "key1=value1;with=semicolons,key2=value2/with/slashes,key3=value3-with-dashes"
  let special_trace_state_context = SpanContext({
    trace_id = "trace-123456789",
    span_id = "span-111111111",
    sampled = true,
    trace_state = special_trace_state
  })
  
  assert_eq(special_trace_state_context.trace_state, special_trace_state)
  
  // Test carrier with special characters
  let special_carrier = TextMapCarrier({ headers = [
    ("x-special-header", "value-with-special-chars!@#$%^&*()"),
    ("x-header-with-spaces", "value with spaces"),
    ("x-header/with/slashes", "value/with/slashes")
  ] })
  
  assert_eq(special_carrier.headers.length(), 3)
  
  // Verify all headers are preserved
  for (key, value) in special_carrier.headers {
    match key {
      "x-special-header" => assert_eq(value, "value-with-special-chars!@#$%^&*()")
      "x-header-with-spaces" => assert_eq(value, "value with spaces")
      "x-header/with/slashes" => assert_eq(value, "value/with/slashes")
      _ => assert_true(false)  // Unexpected key
    }
  }
}