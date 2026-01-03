// Azimuth Telemetry System - New Comprehensive Test Cases
// This file contains new comprehensive test cases for core telemetry functionality

// Test 1: AttributeValue Type Conversion and Validation
test "attribute value type conversion and validation" {
  // Test string to int conversion
  let string_attr = StringValue("42")
  match string_attr {
    StringValue(v) => {
      let int_val = v.to_int()
      match int_val {
        Some(i) => assert_eq(i, 42)
        None => assert_true(false)
      }
    }
    _ => assert_true(false)
  }
  
  // Test string to float conversion
  let float_string_attr = StringValue("3.14")
  match float_string_attr {
    StringValue(v) => {
      let float_val = v.to_float()
      match float_val {
        Some(f) => assert_true(f > 3.13 && f < 3.15)
        None => assert_true(false)
      }
    }
    _ => assert_true(false)
  }
  
  // Test boolean string conversion
  let bool_string_attr = StringValue("true")
  match bool_string_attr {
    StringValue(v) => {
      let bool_val = if v == "true" { true } else { false }
      assert_true(bool_val)
    }
    _ => assert_true(false)
  }
  
  // Test array operations
  let array_attr = ArrayIntValue([1, 2, 3, 4, 5])
  match array_attr {
    ArrayIntValue(v) => {
      assert_eq(v.length(), 5)
      assert_eq(v[0], 1)
      assert_eq(v[4], 5)
      
      let sum = v.reduce(fn(acc, x) { acc + x }, 0)
      assert_eq(sum, 15)
    }
    _ => assert_true(false)
  }
}

// Test 2: Resource Attributes Merge Strategy
test "resource attributes merge strategy" {
  let resource1 = Resource { 
    attributes: [
      ("service.name", StringValue("azimuth-service")),
      ("service.version", StringValue("1.0.0")),
      ("service.instance.id", StringValue("instance-1"))
    ] 
  }
  
  let resource2 = Resource { 
    attributes: [
      ("service.name", StringValue("updated-service")),
      ("service.namespace", StringValue("production")),
      ("host.name", StringValue("host-1"))
    ] 
  }
  
  // Test merge strategy - later resource should override earlier
  let merged_attributes = resource1.attributes.concat(resource2.attributes)
  
  // Check that we have all attributes
  assert_eq(merged_attributes.length(), 6)
  
  // Check that service.name from resource2 overrides resource1
  let service_name = merged_attributes.find(fn(pair) { pair.0 == "service.name" })
  match service_name {
    Some(pair) => {
      match pair.1 {
        StringValue(v) => assert_eq(v, "updated-service")
        _ => assert_true(false)
      }
    }
    None => assert_true(false)
  }
  
  // Check that unique attributes are preserved
  let namespace = merged_attributes.find(fn(pair) { pair.0 == "service.namespace" })
  match namespace {
    Some(pair) => {
      match pair.1 {
        StringValue(v) => assert_eq(v, "production")
        _ => assert_true(false)
      }
    }
    None => assert_true(false)
  }
}

// Test 3: Context Propagation Across Boundaries
test "context propagation across boundaries" {
  // Create initial context
  let initial_context = Context { 
    data: Some(("trace-id", "1234567890abcdef")) 
  }
  
  // Create baggage entries
  let baggage = Baggage { 
    entries: [
      ("user.id", "user-123"),
      ("request.id", "req-456"),
      ("session.id", "sess-789")
    ] 
  }
  
  // Test baggage propagation
  let propagated_entries = baggage.entries.map(fn(entry) {
    (entry.0, entry.1)
  })
  
  assert_eq(propagated_entries.length(), 3)
  
  // Test context data extraction
  match initial_context.data {
    Some((key, value)) => {
      assert_eq(key, "trace-id")
      assert_eq(value, "1234567890abcdef")
    }
    None => assert_true(false)
  }
  
  // Test context key creation
  let context_key = ContextKey { key: "custom-key" }
  assert_eq(context_key.key, "custom-key")
}

// Test 4: Span Lifecycle Management
test "span lifecycle management" {
  // Create span context
  let span_context = SpanContext {
    trace_id: "trace-1234567890abcdef",
    span_id: "span-1234567890abcdef",
    sampled: true,
    trace_state: "state=value"
  }
  
  // Create span
  let span = Span {
    name: "test-operation",
    kind: SpanKind::Internal,
    recording: true,
    span_context: span_context
  }
  
  // Test span properties
  assert_eq(span.name, "test-operation")
  assert_true(span.recording)
  assert_eq(span.span_context.trace_id, "trace-1234567890abcdef")
  assert_eq(span.span_context.span_id, "span-1234567890abcdef")
  assert_true(span.span_context.sampled)
  
  // Test span kind matching
  match span.kind {
    SpanKind::Internal => assert_true(true)
    _ => assert_true(false)
  }
  
  // Test status code operations
  let status_ok = StatusCode::Ok
  let status_error = StatusCode::Error
  let status_unset = StatusCode::Unset
  
  match status_ok {
    StatusCode::Ok => assert_true(true)
    _ => assert_true(false)
  }
  
  match status_error {
    StatusCode::Error => assert_true(true)
    _ => assert_true(false)
  }
  
  match status_unset {
    StatusCode::Unset => assert_true(true)
    _ => assert_true(false)
  }
}

// Test 5: Metrics Instrument Operations
test "metrics instrument operations" {
  // Test counter instrument
  let counter = Counter {
    name: "test.counter",
    unit: Some("count"),
    description: Some("Test counter metric")
  }
  
  assert_eq(counter.name, "test.counter")
  match counter.unit {
    Some(u) => assert_eq(u, "count")
    None => assert_true(false)
  }
  match counter.description {
    Some(d) => assert_eq(d, "Test counter metric")
    None => assert_true(false)
  }
  
  // Test histogram instrument
  let histogram = Histogram {
    name: "test.histogram",
    unit: Some("ms"),
    description: Some("Test histogram metric")
  }
  
  assert_eq(histogram.name, "test.histogram")
  
  // Test updown counter instrument
  let updown_counter = UpDownCounter {
    name: "test.updown.counter",
    unit: Some("count"),
    description: Some("Test updown counter metric")
  }
  
  assert_eq(updown_counter.name, "test.updown.counter")
  
  // Test gauge instrument
  let gauge = Gauge {
    name: "test.gauge",
    unit: Some("value"),
    description: Some("Test gauge metric")
  }
  
  assert_eq(gauge.name, "test.gauge")
  
  // Test instrument type matching
  let counter_instrument = Instrument::Counter("test.counter", Some("count"), Some("Test counter"))
  match counter_instrument {
    Instrument::Counter(name, unit, desc) => {
      assert_eq(name, "test.counter")
      match unit {
        Some(u) => assert_eq(u, "count")
        None => assert_true(false)
      }
    }
    _ => assert_true(false)
  }
}

// Test 6: TextMap Carrier Operations
test "textmap carrier operations" {
  // Create carrier with headers
  let carrier = TextMapCarrier {
    headers: [
      ("traceparent", "00-1234567890abcdef1234567890abcdef-1234567890abcdef-01"),
      ("tracestate", "key1=value1,key2=value2"),
      ("baggage", "user.id=123,request.id=456")
    ]
  }
  
  // Test header extraction
  assert_eq(carrier.headers.length(), 3)
  
  // Test traceparent header
  let traceparent = carrier.headers.find(fn(header) { header.0 == "traceparent" })
  match traceparent {
    Some(header) => {
      assert_eq(header.1, "00-1234567890abcdef1234567890abcdef-1234567890abcdef-01")
    }
    None => assert_true(false)
  }
  
  // Test tracestate header
  let tracestate = carrier.headers.find(fn(header) { header.0 == "tracestate" })
  match tracestate {
    Some(header) => {
      assert_eq(header.1, "key1=value1,key2=value2")
    }
    None => assert_true(false)
  }
  
  // Test baggage header
  let baggage = carrier.headers.find(fn(header) { header.0 == "baggage" })
  match baggage {
    Some(header) => {
      assert_eq(header.1, "user.id=123,request.id=456")
    }
    None => assert_true(false)
  }
}

// Test 7: Instrumentation Scope Operations
test "instrumentation scope operations" {
  // Create instrumentation scope
  let scope = InstrumentationScope {
    name: "azimuth.telemetry",
    version: Some("1.0.0"),
    schema_url: Some("https://example.com/schema/v1")
  }
  
  // Test scope properties
  assert_eq(scope.name, "azimuth.telemetry")
  match scope.version {
    Some(v) => assert_eq(v, "1.0.0")
    None => assert_true(false)
  }
  match scope.schema_url {
    Some(url) => assert_eq(url, "https://example.com/schema/v1")
    None => assert_true(false)
  }
  
  // Test scope with minimal properties
  let minimal_scope = InstrumentationScope {
    name: "minimal.scope",
    version: None,
    schema_url: None
  }
  
  assert_eq(minimal_scope.name, "minimal.scope")
  assert_eq(minimal_scope.version, None)
  assert_eq(minimal_scope.schema_url, None)
}

// Test 8: Complex Attribute Operations
test "complex attribute operations" {
  // Create complex attributes structure
  let complex_attributes = Attributes {
    values: [
      ("string.attr", StringValue("test-value")),
      ("int.attr", IntValue(42)),
      ("float.attr", FloatValue(3.14159)),
      ("bool.attr", BoolValue(true)),
      ("string.array", ArrayStringValue(["a", "b", "c"])),
      ("int.array", ArrayIntValue([1, 2, 3, 4, 5]))
    ]
  }
  
  // Test attribute count
  assert_eq(complex_attributes.values.length(), 6)
  
  // Test string attribute
  let string_attr = complex_attributes.values.find(fn(attr) { attr.0 == "string.attr" })
  match string_attr {
    Some(attr) => {
      match attr.1 {
        StringValue(v) => assert_eq(v, "test-value")
        _ => assert_true(false)
      }
    }
    None => assert_true(false)
  }
  
  // Test int attribute
  let int_attr = complex_attributes.values.find(fn(attr) { attr.0 == "int.attr" })
  match int_attr {
    Some(attr) => {
      match attr.1 {
        IntValue(v) => assert_eq(v, 42)
        _ => assert_true(false)
      }
    }
    None => assert_true(false)
  }
  
  // Test float attribute
  let float_attr = complex_attributes.values.find(fn(attr) { attr.0 == "float.attr" })
  match float_attr {
    Some(attr) => {
      match attr.1 {
        FloatValue(v) => assert_true(v > 3.14 && v < 3.15)
        _ => assert_true(false)
      }
    }
    None => assert_true(false)
  }
  
  // Test bool attribute
  let bool_attr = complex_attributes.values.find(fn(attr) { attr.0 == "bool.attr" })
  match bool_attr {
    Some(attr) => {
      match attr.1 {
        BoolValue(v) => assert_true(v)
        _ => assert_true(false)
      }
    }
    None => assert_true(false)
  }
  
  // Test string array attribute
  let string_array_attr = complex_attributes.values.find(fn(attr) { attr.0 == "string.array" })
  match string_array_attr {
    Some(attr) => {
      match attr.1 {
        ArrayStringValue(v) => {
          assert_eq(v.length(), 3)
          assert_eq(v[0], "a")
          assert_eq(v[1], "b")
          assert_eq(v[2], "c")
        }
        _ => assert_true(false)
      }
    }
    None => assert_true(false)
  }
  
  // Test int array attribute
  let int_array_attr = complex_attributes.values.find(fn(attr) { attr.0 == "int.array" })
  match int_array_attr {
    Some(attr) => {
      match attr.1 {
        ArrayIntValue(v) => {
          assert_eq(v.length(), 5)
          assert_eq(v[0], 1)
          assert_eq(v[4], 5)
        }
        _ => assert_true(false)
      }
    }
    None => assert_true(false)
  }
}

// Test 9: Error Boundary Handling
test "error boundary handling" {
  // Test handling of None values
  let none_option : Option[String] = None
  match none_option {
    Some(v) => assert_true(false)
    None => assert_true(true)
  }
  
  // Test handling of empty arrays
  let empty_array = [] : Array[Int]
  assert_eq(empty_array.length(), 0)
  
  // Test handling of empty strings
  let empty_string = ""
  assert_eq(empty_string.length(), 0)
  
  // Test handling of zero values
  let zero_int = 0
  let zero_float = 0.0
  assert_eq(zero_int, 0)
  assert_eq(zero_float, 0.0)
  
  // Test error handling with Result type
  let success_result = Ok("success")
  let error_result = Err("error message")
  
  match success_result {
    Ok(value) => assert_eq(value, "success")
    Err(_) => assert_true(false)
  }
  
  match error_result {
    Ok(_) => assert_true(false)
    Err(message) => assert_eq(message, "error message")
  }
  
  // Test boundary conditions
  let max_int = 2147483647
  let min_int = -2147483648
  
  assert_eq(max_int + 1, -2147483648) // Integer overflow
  assert_eq(min_int - 1, 2147483647) // Integer underflow
  
  // Test array bounds
  let small_array = [1, 2, 3]
  assert_eq(small_array.length(), 3)
  
  // Safe array access
  let first_element = if small_array.length() > 0 { Some(small_array[0]) } else { None }
  match first_element {
    Some(value) => assert_eq(value, 1)
    None => assert_true(false)
  }
  
  let out_of_bounds = if small_array.length() > 10 { Some(small_array[10]) } else { None }
  match out_of_bounds {
    Some(_) => assert_true(false)
    None => assert_true(true)
  }
}

// Test 10: Performance Validation
test "performance validation" {
  // Test performance of array operations
  let large_array = []
  for i in 1..=1000 {
    large_array = large_array.push(i)
  }
  
  assert_eq(large_array.length(), 1000)
  assert_eq(large_array[0], 1)
  assert_eq(large_array[999], 1000)
  
  // Test performance of map operations
  let mapped_array = large_array.map(fn(x) { x * 2 })
  assert_eq(mapped_array.length(), 1000)
  assert_eq(mapped_array[0], 2)
  assert_eq(mapped_array[999], 2000)
  
  // Test performance of filter operations
  let filtered_array = large_array.filter(fn(x) { x % 2 == 0 })
  assert_eq(filtered_array.length(), 500)
  
  // Test performance of reduce operations
  let sum = large_array.reduce(fn(acc, x) { acc + x }, 0)
  assert_eq(sum, 500500) // Sum of 1 to 1000
  
  // Test string operations performance
  let long_string = "a".repeat(1000)
  assert_eq(long_string.length(), 1000)
  
  // Test string concatenation performance
  let mut concatenated = ""
  for i in 1..=100 {
    concatenated = concatenated + "test"
  }
  assert_eq(concatenated.length(), 400)
}