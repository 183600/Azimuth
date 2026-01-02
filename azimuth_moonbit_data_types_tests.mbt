// Azimuth Telemetry System - MoonBit Data Types Tests
// This file contains test cases for basic data types and type conversions

// Test 1: AttributeValue enum variants
test "attribute value enum variants" {
  // Test StringValue
  let string_attr = StringValue("test_string")
  match string_attr {
    StringValue(v) => assert_eq(v, "test_string")
    _ => assert_true(false)
  }
  
  // Test IntValue
  let int_attr = IntValue(42)
  match int_attr {
    IntValue(v) => assert_eq(v, 42)
    _ => assert_true(false)
  }
  
  // Test FloatValue
  let float_attr = FloatValue(3.14159)
  match float_attr {
    FloatValue(v) => assert_eq(v, 3.14159)
    _ => assert_true(false)
  }
  
  // Test BoolValue
  let bool_attr = BoolValue(true)
  match bool_attr {
    BoolValue(v) => assert_true(v)
    _ => assert_true(false)
  }
  
  // Test ArrayStringValue
  let array_string_attr = ArrayStringValue(["a", "b", "c"])
  match array_string_attr {
    ArrayStringValue(v) => {
      assert_eq(v.length(), 3)
      assert_eq(v[0], "a")
      assert_eq(v[1], "b")
      assert_eq(v[2], "c")
    }
    _ => assert_true(false)
  }
  
  // Test ArrayIntValue
  let array_int_attr = ArrayIntValue([1, 2, 3, 4, 5])
  match array_int_attr {
    ArrayIntValue(v) => {
      assert_eq(v.length(), 5)
      assert_eq(v[0], 1)
      assert_eq(v[4], 5)
    }
    _ => assert_true(false)
  }
}

// Test 2: Attributes struct operations
test "attributes struct operations" {
  let attrs = Attributes({ values = [
    ("key1", StringValue("value1")),
    ("key2", IntValue(123)),
    ("key3", BoolValue(true))
  ] })
  
  assert_eq(attrs.values.length(), 3)
  
  // Test first attribute
  let (key1, value1) = attrs.values[0]
  assert_eq(key1, "key1")
  match value1 {
    StringValue(v) => assert_eq(v, "value1")
    _ => assert_true(false)
  }
  
  // Test second attribute
  let (key2, value2) = attrs.values[1]
  assert_eq(key2, "key2")
  match value2 {
    IntValue(v) => assert_eq(v, 123)
    _ => assert_true(false)
  }
  
  // Test third attribute
  let (key3, value3) = attrs.values[2]
  assert_eq(key3, "key3")
  match value3 {
    BoolValue(v) => assert_true(v)
    _ => assert_true(false)
  }
}

// Test 3: Resource struct operations
test "resource struct operations" {
  let resource = Resource({ attributes = [
    ("service.name", StringValue("azimuth-service")),
    ("service.version", StringValue("1.0.0")),
    ("service.instance.id", StringValue("instance-123"))
  ] })
  
  assert_eq(resource.attributes.length(), 3)
  
  // Verify service name
  let (name_key, name_value) = resource.attributes[0]
  assert_eq(name_key, "service.name")
  match name_value {
    StringValue(v) => assert_eq(v, "azimuth-service")
    _ => assert_true(false)
  }
  
  // Verify service version
  let (version_key, version_value) = resource.attributes[1]
  assert_eq(version_key, "service.version")
  match version_value {
    StringValue(v) => assert_eq(v, "1.0.0")
    _ => assert_true(false)
  }
}

// Test 4: InstrumentationScope struct operations
test "instrumentation scope struct operations" {
  let scope_full = InstrumentationScope({
    name = "azimuth.instrument",
    version = Some("2.0.0"),
    schema_url = Some("https://example.com/schema")
  })
  
  assert_eq(scope_full.name, "azimuth.instrument")
  match scope_full.version {
    Some(v) => assert_eq(v, "2.0.0")
    None => assert_true(false)
  }
  match scope_full.schema_url {
    Some(url) => assert_eq(url, "https://example.com/schema")
    None => assert_true(false)
  }
  
  // Test with optional values as None
  let scope_minimal = InstrumentationScope({
    name = "minimal.instrument",
    version = None,
    schema_url = None
  })
  
  assert_eq(scope_minimal.name, "minimal.instrument")
  match scope_minimal.version {
    Some(_) => assert_true(false)
    None => assert_true(true)
  }
  match scope_minimal.schema_url {
    Some(_) => assert_true(false)
    None => assert_true(true)
  }
}

// Test 5: Context struct operations
test "context struct operations" {
  let context_with_data = Context({ data = Some("context_key", "context_value") })
  
  match context_with_data.data {
    Some((key, value)) => {
      assert_eq(key, "context_key")
      assert_eq(value, "context_value")
    }
    None => assert_true(false)
  }
  
  let context_empty = Context({ data = None })
  match context_empty.data {
    Some(_) => assert_true(false)
    None => assert_true(true)
  }
}

// Test 6: ContextKey operations
test "context key operations" {
  let string_key = ContextKey[String]({ key = "string_context_key" })
  let int_key = ContextKey[Int]({ key = "int_context_key" })
  
  assert_eq(string_key.key, "string_context_key")
  assert_eq(int_key.key, "int_context_key")
}

// Test 7: Baggage struct operations
test "baggage struct operations" {
  let baggage = Baggage({ entries = [
    ("user.id", "12345"),
    ("request.id", "req-67890"),
    ("trace.origin", "frontend")
  ] })
  
  assert_eq(baggage.entries.length(), 3)
  
  // Verify first entry
  let (key1, value1) = baggage.entries[0]
  assert_eq(key1, "user.id")
  assert_eq(value1, "12345")
  
  // Verify second entry
  let (key2, value2) = baggage.entries[1]
  assert_eq(key2, "request.id")
  assert_eq(value2, "req-67890")
  
  // Verify third entry
  let (key3, value3) = baggage.entries[2]
  assert_eq(key3, "trace.origin")
  assert_eq(value3, "frontend")
}

// Test 8: SpanContext struct operations
test "span context struct operations" {
  let span_context = SpanContext({
    trace_id = "trace-123456789",
    span_id = "span-987654321",
    sampled = true,
    trace_state = "key1=value1,key2=value2"
  })
  
  assert_eq(span_context.trace_id, "trace-123456789")
  assert_eq(span_context.span_id, "span-987654321")
  assert_true(span_context.sampled)
  assert_eq(span_context.trace_state, "key1=value1,key2=value2")
  
  // Test with not sampled
  let span_context_not_sampled = SpanContext({
    trace_id = "trace-111111111",
    span_id = "span-222222222",
    sampled = false,
    trace_state = ""
  })
  
  assert_eq(span_context_not_sampled.trace_id, "trace-111111111")
  assert_eq(span_context_not_sampled.span_id, "span-222222222")
  assert_false(span_context_not_sampled.sampled)
  assert_eq(span_context_not_sampled.trace_state, "")
}

// Test 9: TextMapCarrier struct operations
test "text map carrier struct operations" {
  let carrier = TextMapCarrier({ headers = [
    ("traceparent", "00-trace-id-span-id-01"),
    ("x-correlation-id", "correlation-123"),
    ("baggage", "key1=value1,key2=value2")
  ] })
  
  assert_eq(carrier.headers.length(), 3)
  
  // Verify traceparent header
  let (key1, value1) = carrier.headers[0]
  assert_eq(key1, "traceparent")
  assert_eq(value1, "00-trace-id-span-id-01")
  
  // Verify correlation ID header
  let (key2, value2) = carrier.headers[1]
  assert_eq(key2, "x-correlation-id")
  assert_eq(value2, "correlation-123")
  
  // Verify baggage header
  let (key3, value3) = carrier.headers[2]
  assert_eq(key3, "baggage")
  assert_eq(value3, "key1=value1,key2=value2")
}

// Test 10: Type conversion edge cases
test "type conversion edge cases" {
  // Test empty arrays
  let empty_string_array = ArrayStringValue([])
  match empty_string_array {
    ArrayStringValue(v) => assert_eq(v.length(), 0)
    _ => assert_true(false)
  }
  
  let empty_int_array = ArrayIntValue([])
  match empty_int_array {
    ArrayIntValue(v) => assert_eq(v.length(), 0)
    _ => assert_true(false)
  }
  
  // Test empty attributes
  let empty_attrs = Attributes({ values = [] })
  assert_eq(empty_attrs.values.length(), 0)
  
  // Test empty resource
  let empty_resource = Resource({ attributes = [] })
  assert_eq(empty_resource.attributes.length(), 0)
  
  // Test empty baggage
  let empty_baggage = Baggage({ entries = [] })
  assert_eq(empty_baggage.entries.length(), 0)
  
  // Test empty carrier
  let empty_carrier = TextMapCarrier({ headers = [] })
  assert_eq(empty_carrier.headers.length(), 0)
}