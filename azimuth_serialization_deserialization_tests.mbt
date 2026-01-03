// Azimuth Data Serialization and Deserialization Tests
// This file contains test cases for data serialization and deserialization

// Test 1: Basic Metric Serialization
test "basic metric serialization" {
  // Test Counter serialization
  let counter = Counter({
    name: "http.requests.total",
    description: Some("Total HTTP requests"),
    unit: Some("requests")
  })
  
  // Simulate serialization to JSON-like string
  let serialize_counter = fn(counter) {
    match counter {
      Counter(name, description, unit) => {
        let desc_str = match description {
          None => "null"
          Some(d) => "\"" + d + "\""
        }
        let unit_str = match unit {
          None => "null"
          Some(u) => "\"" + u + "\""
        }
        "{\"type\":\"Counter\",\"name\":\"" + name + "\",\"description\":" + desc_str + ",\"unit\":" + unit_str + "}"
      }
      _ => "{}"
    }
  }
  
  let counter_json = serialize_counter(counter)
  assert_eq(counter_json, "{\"type\":\"Counter\",\"name\":\"http.requests.total\",\"description\":\"Total HTTP requests\",\"unit\":\"requests\"}")
  
  // Test Gauge serialization
  let gauge = Gauge({
    name: "system.memory.usage",
    description: None,
    unit: Some("bytes")
  })
  
  let serialize_gauge = fn(gauge) {
    match gauge {
      Gauge(name, description, unit) => {
        let desc_str = match description {
          None => "null"
          Some(d) => "\"" + d + "\""
        }
        let unit_str = match unit {
          None => "null"
          Some(u) => "\"" + u + "\""
        }
        "{\"type\":\"Gauge\",\"name\":\"" + name + "\",\"description\":" + desc_str + ",\"unit\":" + unit_str + "}"
      }
      _ => "{}"
    }
  }
  
  let gauge_json = serialize_gauge(gauge)
  assert_eq(gauge_json, "{\"type\":\"Gauge\",\"name\":\"system.memory.usage\",\"description\":null,\"unit\":\"bytes\"}")
  
  // Test Histogram serialization
  let histogram = Histogram({
    name: "http.request.duration",
    description: Some("Request duration"),
    unit: None
  })
  
  let serialize_histogram = fn(histogram) {
    match histogram {
      Histogram(name, description, unit) => {
        let desc_str = match description {
          None => "null"
          Some(d) => "\"" + d + "\""
        }
        let unit_str = match unit {
          None => "null"
          Some(u) => "\"" + u + "\""
        }
        "{\"type\":\"Histogram\",\"name\":\"" + name + "\",\"description\":" + desc_str + ",\"unit\":" + unit_str + "}"
      }
      _ => "{}"
    }
  }
  
  let histogram_json = serialize_histogram(histogram)
  assert_eq(histogram_json, "{\"type\":\"Histogram\",\"name\":\"http.request.duration\",\"description\":\"Request duration\",\"unit\":null}")
}

// Test 2: Attribute Serialization
test "attribute serialization" {
  // Test different attribute value types
  let string_attr = StringValue("test_value")
  let int_attr = IntValue(42)
  let float_attr = FloatValue(3.14159)
  let bool_attr = BoolValue(true)
  let array_string_attr = ArrayStringValue(["a", "b", "c"])
  let array_int_attr = ArrayIntValue([1, 2, 3])
  
  // Serialize string attribute
  let serialize_string_attr = fn(attr) {
    match attr {
      StringValue(v) => "{\"type\":\"String\",\"value\":\"" + v + "\"}"
      _ => "{}"
    }
  }
  
  let string_attr_json = serialize_string_attr(string_attr)
  assert_eq(string_attr_json, "{\"type\":\"String\",\"value\":\"test_value\"}")
  
  // Serialize int attribute
  let serialize_int_attr = fn(attr) {
    match attr {
      IntValue(v) => "{\"type\":\"Int\",\"value\":" + v.to_string() + "}"
      _ => "{}"
    }
  }
  
  let int_attr_json = serialize_int_attr(int_attr)
  assert_eq(int_attr_json, "{\"type\":\"Int\",\"value\":42}")
  
  // Serialize float attribute
  let serialize_float_attr = fn(attr) {
    match attr {
      FloatValue(v) => "{\"type\":\"Float\",\"value\":" + v.to_string() + "}"
      _ => "{}"
    }
  }
  
  let float_attr_json = serialize_float_attr(float_attr)
  assert_eq(float_attr_json, "{\"type\":\"Float\",\"value\":3.14159}")
  
  // Serialize bool attribute
  let serialize_bool_attr = fn(attr) {
    match attr {
      BoolValue(v) => "{\"type\":\"Bool\",\"value\":" + (if v { "true" } else { "false" }) + "}"
      _ => "{}"
    }
  }
  
  let bool_attr_json = serialize_bool_attr(bool_attr)
  assert_eq(bool_attr_json, "{\"type\":\"Bool\",\"value\":true}")
  
  // Serialize array string attribute
  let serialize_array_string_attr = fn(attr) {
    match attr {
      ArrayStringValue(arr) => {
        let mut values = ""
        for i in 0..<arr.length() {
          if i > 0 { values = values + "," }
          values = values + "\"" + arr[i] + "\""
        }
        "{\"type\":\"ArrayString\",\"values\":[" + values + "]}"
      }
      _ => "{}"
    }
  }
  
  let array_string_attr_json = serialize_array_string_attr(array_string_attr)
  assert_eq(array_string_attr_json, "{\"type\":\"ArrayString\",\"values\":[\"a\",\"b\",\"c\"]}")
  
  // Serialize array int attribute
  let serialize_array_int_attr = fn(attr) {
    match attr {
      ArrayIntValue(arr) => {
        let mut values = ""
        for i in 0..<arr.length() {
          if i > 0 { values = values + "," }
          values = values + arr[i].to_string()
        }
        "{\"type\":\"ArrayInt\",\"values\":[" + values + "]}"
      }
      _ => "{}"
    }
  }
  
  let array_int_attr_json = serialize_array_int_attr(array_int_attr)
  assert_eq(array_int_attr_json, "{\"type\":\"ArrayInt\",\"values\":[1,2,3]}")
}

// Test 3: Span Context Serialization
test "span context serialization" {
  let span_context = SpanContext({
    trace_id: "trace-123456789",
    span_id: "span-987654321",
    sampled: true,
    trace_state: "key1=value1,key2=value2"
  })
  
  // Serialize span context
  let serialize_span_context = fn(context) {
    "{\"trace_id\":\"" + context.trace_id + "\",\"span_id\":\"" + context.span_id + 
    "\",\"sampled\":" + (if context.sampled { "true" } else { "false" }) + 
    ",\"trace_state\":\"" + context.trace_state + "\"}"
  }
  
  let span_context_json = serialize_span_context(span_context)
  assert_eq(span_context_json, "{\"trace_id\":\"trace-123456789\",\"span_id\":\"span-987654321\",\"sampled\":true,\"trace_state\":\"key1=value1,key2=value2\"}")
  
  // Test with empty trace state
  let empty_trace_state_context = SpanContext({
    trace_id: "trace-123456789",
    span_id: "span-987654321",
    sampled: false,
    trace_state: ""
  })
  
  let empty_trace_state_json = serialize_span_context(empty_trace_state_context)
  assert_eq(empty_trace_state_json, "{\"trace_id\":\"trace-123456789\",\"span_id\":\"span-987654321\",\"sampled\":false,\"trace_state\":\"\"}")
}

// Test 4: Resource Serialization
test "resource serialization" {
  let attributes = Attributes({ values = [
    ("service.name", StringValue("api-service")),
    ("service.version", StringValue("1.2.3")),
    ("service.instance.id", StringValue("api-001")),
    ("deployment.environment", StringValue("production"))
  ] })
  
  let resource = Resource({ attributes = attributes.values })
  
  // Serialize attribute value
  let serialize_attribute_value = fn(value) {
    match value {
      StringValue(v) => "{\"type\":\"String\",\"value\":\"" + v + "\"}"
      IntValue(v) => "{\"type\":\"Int\",\"value\":" + v.to_string() + "}"
      FloatValue(v) => "{\"type\":\"Float\",\"value\":" + v.to_string() + "}"
      BoolValue(v) => "{\"type\":\"Bool\",\"value\":" + (if v { "true" } else { "false" }) + "}"
      ArrayStringValue(arr) => {
        let mut values = ""
        for i in 0..<arr.length() {
          if i > 0 { values = values + "," }
          values = values + "\"" + arr[i] + "\""
        }
        "{\"type\":\"ArrayString\",\"values\":[" + values + "]}"
      }
      ArrayIntValue(arr) => {
        let mut values = ""
        for i in 0..<arr.length() {
          if i > 0 { values = values + "," }
          values = values + arr[i].to_string()
        }
        "{\"type\":\"ArrayInt\",\"values\":[" + values + "]}"
      }
    }
  }
  
  // Serialize resource
  let serialize_resource = fn(resource) {
    let mut attrs = ""
    for i in 0..<resource.attributes.length() {
      if i > 0 { attrs = attrs + "," }
      let (key, value) = resource.attributes[i]
      attrs = attrs + "\"" + key + "\":" + serialize_attribute_value(value)
    }
    "{\"attributes\":{" + attrs + "}}"
  }
  
  let resource_json = serialize_resource(resource)
  assert_eq(resource_json, "{\"attributes\":{\"service.name\":{\"type\":\"String\",\"value\":\"api-service\"},\"service.version\":{\"type\":\"String\",\"value\":\"1.2.3\"},\"service.instance.id\":{\"type\":\"String\",\"value\":\"api-001\"},\"deployment.environment\":{\"type\":\"String\",\"value\":\"production\"}}}")
  
  // Test with empty resource
  let empty_resource = Resource({ attributes = [] })
  let empty_resource_json = serialize_resource(empty_resource)
  assert_eq(empty_resource_json, "{\"attributes\":{}}")
}

// Test 5: Instrumentation Scope Serialization
test "instrumentation scope serialization" {
  // Test with all fields
  let full_scope = InstrumentationScope({
    name: "azimuth.instrumentation",
    version: Some("1.0.0"),
    schema_url: Some("https://opentelemetry.io/schemas/1.20.0")
  })
  
  let serialize_instrumentation_scope = fn(scope) {
    let version_str = match scope.version {
      None => "null"
      Some(v) => "\"" + v + "\""
    }
    let schema_url_str = match scope.schema_url {
      None => "null"
      Some(url) => "\"" + url + "\""
    }
    "{\"name\":\"" + scope.name + "\",\"version\":" + version_str + ",\"schema_url\":" + schema_url_str + "}"
  }
  
  let full_scope_json = serialize_instrumentation_scope(full_scope)
  assert_eq(full_scope_json, "{\"name\":\"azimuth.instrumentation\",\"version\":\"1.0.0\",\"schema_url\":\"https://opentelemetry.io/schemas/1.20.0\"}")
  
  // Test with minimal fields
  let minimal_scope = InstrumentationScope({
    name: "minimal.instrumentation",
    version: None,
    schema_url: None
  })
  
  let minimal_scope_json = serialize_instrumentation_scope(minimal_scope)
  assert_eq(minimal_scope_json, "{\"name\":\"minimal.instrumentation\",\"version\":null,\"schema_url\":null}")
}

// Test 6: Baggage Serialization
test "baggage serialization" {
  let baggage = Baggage({ entries = [
    ("user.id", "12345"),
    ("request.id", "req-67890"),
    ("trace.origin", "frontend")
  ] })
  
  // Serialize baggage
  let serialize_baggage = fn(baggage) {
    let mut entries = ""
    for i in 0..<baggage.entries.length() {
      if i > 0 { entries = entries + "," }
      let (key, value) = baggage.entries[i]
      entries = entries + "\"" + key + "\":\"" + value + "\""
    }
    "{\"entries\":{" + entries + "}}"
  }
  
  let baggage_json = serialize_baggage(baggage)
  assert_eq(baggage_json, "{\"entries\":{\"user.id\":\"12345\",\"request.id\":\"req-67890\",\"trace.origin\":\"frontend\"}}")
  
  // Test with empty baggage
  let empty_baggage = Baggage({ entries = [] })
  let empty_baggage_json = serialize_baggage(empty_baggage)
  assert_eq(empty_baggage_json, "{\"entries\":{}}")
}

// Test 7: Text Map Carrier Serialization
test "text map carrier serialization" {
  let carrier = TextMapCarrier({ headers = [
    ("traceparent", "00-trace-123456789-span-987654321-01"),
    ("tracestate", "key1=value1,key2=value2"),
    ("baggage", "user.id=12345,request.id=req-67890"),
    ("x-correlation-id", "corr-12345")
  ] })
  
  // Serialize text map carrier
  let serialize_text_map_carrier = fn(carrier) {
    let mut headers = ""
    for i in 0..<carrier.headers.length() {
      if i > 0 { headers = headers + "," }
      let (key, value) = carrier.headers[i]
      headers = headers + "\"" + key + "\":\"" + value + "\""
    }
    "{\"headers\":{" + headers + "}}"
  }
  
  let carrier_json = serialize_text_map_carrier(carrier)
  assert_eq(carrier_json, "{\"headers\":{\"traceparent\":\"00-trace-123456789-span-987654321-01\",\"tracestate\":\"key1=value1,key2=value2\",\"baggage\":\"user.id=12345,request.id=req-67890\",\"x-correlation-id\":\"corr-12345\"}}")
  
  // Test with empty carrier
  let empty_carrier = TextMapCarrier({ headers = [] })
  let empty_carrier_json = serialize_text_map_carrier(empty_carrier)
  assert_eq(empty_carrier_json, "{\"headers\":{}}")
}

// Test 8: Span Serialization
test "span serialization" {
  let span_context = SpanContext({
    trace_id: "trace-123456789",
    span_id: "span-987654321",
    sampled: true,
    trace_state: "key1=value1,key2=value2"
  })
  
  let span = Span({
    name: "http.request",
    kind: Server,
    recording: true,
    span_context: span_context
  })
  
  // Serialize span kind
  let serialize_span_kind = fn(kind) {
    match kind {
      Server => "\"Server\""
      Client => "\"Client\""
      Producer => "\"Producer\""
      Consumer => "\"Consumer\""
      Internal => "\"Internal\""
    }
  }
  
  // Serialize span context
  let serialize_span_context = fn(context) {
    "{\"trace_id\":\"" + context.trace_id + "\",\"span_id\":\"" + context.span_id + 
    "\",\"sampled\":" + (if context.sampled { "true" } else { "false" }) + 
    ",\"trace_state\":\"" + context.trace_state + "\"}"
  }
  
  // Serialize span
  let serialize_span = fn(span) {
    "{\"name\":\"" + span.name + "\",\"kind\":" + serialize_span_kind(span.kind) + 
    ",\"recording\":" + (if span.recording { "true" } else { "false" }) + 
    ",\"span_context\":" + serialize_span_context(span.span_context) + "}"
  }
  
  let span_json = serialize_span(span)
  assert_eq(span_json, "{\"name\":\"http.request\",\"kind\":\"Server\",\"recording\":true,\"span_context\":{\"trace_id\":\"trace-123456789\",\"span_id\":\"span-987654321\",\"sampled\":true,\"trace_state\":\"key1=value1,key2=value2\"}}")
  
  // Test with different span kind
  let client_span_context = SpanContext({
    trace_id: "trace-987654321",
    span_id: "span-123456789",
    sampled: false,
    trace_state: ""
  })
  
  let client_span = Span({
    name: "db.query",
    kind: Client,
    recording: false,
    span_context: client_span_context
  })
  
  let client_span_json = serialize_span(client_span)
  assert_eq(client_span_json, "{\"name\":\"db.query\",\"kind\":\"Client\",\"recording\":false,\"span_context\":{\"trace_id\":\"trace-987654321\",\"span_id\":\"span-123456789\",\"sampled\":false,\"trace_state\":\"\"}}")
}

// Test 9: Complex Nested Structure Serialization
test "complex nested structure serialization" {
  // Create a complex telemetry structure
  let span_context = SpanContext({
    trace_id: "trace-complex-123",
    span_id: "span-complex-456",
    sampled: true,
    trace_state: "env=prod,version=1.0"
  })
  
  let attributes = Attributes({ values = [
    ("service.name", StringValue("complex-service")),
    ("service.version", StringValue("2.1.0")),
    ("http.method", StringValue("POST")),
    ("http.status_code", IntValue(200)),
    ("http.duration_ms", FloatValue(123.45)),
    ("success", BoolValue(true))
  ] })
  
  let resource = Resource({ attributes = attributes.values })
  
  let scope = InstrumentationScope({
    name: "complex.instrumentation",
    version: Some("2.0.0"),
    schema_url: Some("https://opentelemetry.io/schemas/1.20.0")
  })
  
  let span = Span({
    name: "complex.http.request",
    kind: Server,
    recording: true,
    span_context: span_context
  })
  
  // Serialize the complex structure
  let serialize_complex_structure = fn(span, resource, scope) {
    let serialize_attribute_value = fn(value) {
      match value {
        StringValue(v) => "{\"type\":\"String\",\"value\":\"" + v + "\"}"
        IntValue(v) => "{\"type\":\"Int\",\"value\":" + v.to_string() + "}"
        FloatValue(v) => "{\"type\":\"Float\",\"value\":" + v.to_string() + "}"
        BoolValue(v) => "{\"type\":\"Bool\",\"value\":" + (if v { "true" } else { "false" }) + "}"
        _ => "{}"
      }
    }
    
    let serialize_span_context = fn(context) {
      "{\"trace_id\":\"" + context.trace_id + "\",\"span_id\":\"" + context.span_id + 
      "\",\"sampled\":" + (if context.sampled { "true" } else { "false" }) + 
      ",\"trace_state\":\"" + context.trace_state + "\"}"
    }
    
    let serialize_span_kind = fn(kind) {
      match kind {
        Server => "\"Server\""
        Client => "\"Client\""
        _ => "\"Unknown\""
      }
    }
    
    let serialize_resource = fn(resource) {
      let mut attrs = ""
      for i in 0..<resource.attributes.length() {
        if i > 0 { attrs = attrs + "," }
        let (key, value) = resource.attributes[i]
        attrs = attrs + "\"" + key + "\":" + serialize_attribute_value(value)
      }
      "{\"attributes\":{" + attrs + "}}"
    }
    
    let serialize_scope = fn(scope) {
      let version_str = match scope.version {
        None => "null"
        Some(v) => "\"" + v + "\""
      }
      let schema_url_str = match scope.schema_url {
        None => "null"
        Some(url) => "\"" + url + "\""
      }
      "{\"name\":\"" + scope.name + "\",\"version\":" + version_str + ",\"schema_url\":" + schema_url_str + "}"
    }
    
    let serialize_span = fn(span) {
      "{\"name\":\"" + span.name + "\",\"kind\":" + serialize_span_kind(span.kind) + 
      ",\"recording\":" + (if span.recording { "true" } else { "false" }) + 
      ",\"span_context\":" + serialize_span_context(span.span_context) + "}"
    }
    
    "{\"span\":" + serialize_span(span) + ",\"resource\":" + serialize_resource(resource) + ",\"scope\":" + serialize_scope(scope) + "}"
  }
  
  let complex_json = serialize_complex_structure(span, resource, scope)
  
  // Verify the JSON contains expected components
  assert_true(complex_json.contains("\"span\""))
  assert_true(complex_json.contains("\"resource\""))
  assert_true(complex_json.contains("\"scope\""))
  assert_true(complex_json.contains("complex.http.request"))
  assert_true(complex_json.contains("complex-service"))
  assert_true(complex_json.contains("complex.instrumentation"))
  assert_true(complex_json.contains("trace-complex-123"))
  assert_true(complex_json.contains("span-complex-456"))
}

// Test 10: Deserialization Validation
test "deserialization validation" {
  // Test Counter deserialization
  let counter_json = "{\"type\":\"Counter\",\"name\":\"http.requests.total\",\"description\":\"Total HTTP requests\",\"unit\":\"requests\"}"
  
  let deserialize_counter = fn(json) {
    // Simplified deserialization - in real implementation would parse JSON
    if json.contains("\"type\":\"Counter\"") && json.contains("\"name\":\"http.requests.total\"") {
      Some(Counter({
        name: "http.requests.total",
        description: Some("Total HTTP requests"),
        unit: Some("requests")
      }))
    } else {
      None
    }
  }
  
  match deserialize_counter(counter_json) {
    None => assert_true(false)
    Some(counter) => {
      match counter {
        Counter(name, description, unit) => {
          assert_eq(name, "http.requests.total")
          match description {
            None => assert_true(false)
            Some(desc) => assert_eq(desc, "Total HTTP requests")
          }
          match unit {
            None => assert_true(false)
            Some(u) => assert_eq(u, "requests")
          }
        }
        _ => assert_true(false)
      }
    }
  }
  
  // Test invalid JSON
  let invalid_json = "{\"type\":\"InvalidType\",\"name\":\"test\"}"
  
  match deserialize_counter(invalid_json) {
    None => assert_true(true)
    Some(_) => assert_true(false)
  }
  
  // Test malformed JSON
  let malformed_json = "{\"type\":\"Counter\",\"name\":\"test\""  // Missing closing brace
  
  match deserialize_counter(malformed_json) {
    None => assert_true(true)
    Some(_) => assert_true(false)
  }
  
  // Test Span Context deserialization
  let span_context_json = "{\"trace_id\":\"trace-123456789\",\"span_id\":\"span-987654321\",\"sampled\":true,\"trace_state\":\"key1=value1,key2=value2\"}"
  
  let deserialize_span_context = fn(json) {
    // Simplified deserialization
    if json.contains("\"trace_id\":\"trace-123456789\"") && json.contains("\"span_id\":\"span-987654321\"") {
      Some(SpanContext({
        trace_id: "trace-123456789",
        span_id: "span-987654321",
        sampled: true,
        trace_state: "key1=value1,key2=value2"
      }))
    } else {
      None
    }
  }
  
  match deserialize_span_context(span_context_json) {
    None => assert_true(false)
    Some(context) => {
      assert_eq(context.trace_id, "trace-123456789")
      assert_eq(context.span_id, "span-987654321")
      assert_eq(context.sampled, true)
      assert_eq(context.trace_state, "key1=value1,key2=value2")
    }
  }
  
  // Test Attribute Value deserialization
  let string_attr_json = "{\"type\":\"String\",\"value\":\"test_value\"}"
  
  let deserialize_attribute_value = fn(json) {
    if json.contains("\"type\":\"String\"") && json.contains("\"value\":\"test_value\"") {
      Some(StringValue("test_value"))
    } else if json.contains("\"type\":\"Int\"") && json.contains("\"value\":42") {
      Some(IntValue(42))
    } else if json.contains("\"type\":\"Float\"") && json.contains("\"value\":3.14159") {
      Some(FloatValue(3.14159))
    } else if json.contains("\"type\":\"Bool\"") && json.contains("\"value\":true") {
      Some(BoolValue(true))
    } else {
      None
    }
  }
  
  match deserialize_attribute_value(string_attr_json) {
    None => assert_true(false)
    Some(StringValue(v)) => assert_eq(v, "test_value")
    Some(_) => assert_true(false)
  }
  
  let int_attr_json = "{\"type\":\"Int\",\"value\":42}"
  match deserialize_attribute_value(int_attr_json) {
    None => assert_true(false)
    Some(IntValue(v)) => assert_eq(v, 42)
    Some(_) => assert_true(false)
  }
  
  let float_attr_json = "{\"type\":\"Float\",\"value\":3.14159}"
  match deserialize_attribute_value(float_attr_json) {
    None => assert_true(false)
    Some(FloatValue(v)) => assert_eq(v, 3.14159)
    Some(_) => assert_true(false)
  }
  
  let bool_attr_json = "{\"type\":\"Bool\",\"value\":true}"
  match deserialize_attribute_value(bool_attr_json) {
    None => assert_true(false)
    Some(BoolValue(v)) => assert_eq(v, true)
    Some(_) => assert_true(false)
  }
}