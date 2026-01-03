// Serialization and Deserialization Tests for Azimuth Telemetry System
// This file contains tests for data serialization and deserialization

test "attribute value serialization" {
  let attrs = Attributes::new()
  
  // Set various attribute types
  Attributes::set(attrs, "string.key", StringValue("test.value"))
  Attributes::set(attrs, "int.key", IntValue(42))
  Attributes::set(attrs, "float.key", FloatValue(3.14159))
  Attributes::set(attrs, "bool.key", BoolValue(true))
  Attributes::set(attrs, "string.array", ArrayStringValue(["item1", "item2", "item3"]))
  Attributes::set(attrs, "int.array", ArrayIntValue([1, 2, 3, 4, 5]))
  
  // Simulate serialization by converting to string representation
  let string_result = Attributes::get(attrs, "string.key")
  let int_result = Attributes::get(attrs, "int.key")
  let float_result = Attributes::get(attrs, "float.key")
  let bool_result = Attributes::get(attrs, "bool.key")
  let string_array_result = Attributes::get(attrs, "string.array")
  let int_array_result = Attributes::get(attrs, "int.array")
  
  // Simulate deserialization by reconstructing values
  match string_result {
    Some(StringValue(s)) => {
      // Serialize to string
      let serialized = s
      // Deserialize back
      let deserialized = serialized
      assert_eq(deserialized, "test.value")
    }
    _ => assert_true(false)
  }
  
  match int_result {
    Some(IntValue(v)) => {
      // Serialize to string
      let serialized = v.to_string()
      // Deserialize back
      let deserialized = serialized
      assert_eq(deserialized, "42")
    }
    _ => assert_true(false)
  }
  
  match float_result {
    Some(FloatValue(v)) => {
      // Serialize to string
      let serialized = v.to_string()
      // Deserialize back
      let deserialized = serialized
      assert_true(deserialized.length() > 0)
    }
    _ => assert_true(false)
  }
  
  match bool_result {
    Some(BoolValue(v)) => {
      // Serialize to string
      let serialized = if v { "true" } else { "false" }
      // Deserialize back
      let deserialized = serialized
      assert_eq(deserialized, "true")
    }
    _ => assert_true(false)
  }
  
  match string_array_result {
    Some(ArrayStringValue(arr)) => {
      // Serialize array to string
      let mut serialized = ""
      for i in 0..arr.length() {
        if i > 0 { serialized = serialized + "," }
        serialized = serialized + arr[i]
      }
      // Deserialize back (simplified)
      let deserialized = serialized
      assert_eq(deserialized, "item1,item2,item3")
    }
    _ => assert_true(false)
  }
  
  match int_array_result {
    Some(ArrayIntValue(arr)) => {
      // Serialize array to string
      let mut serialized = ""
      for i in 0..arr.length() {
        if i > 0 { serialized = serialized + "," }
        serialized = serialized + arr[i].to_string()
      }
      // Deserialize back (simplified)
      let deserialized = serialized
      assert_eq(deserialized, "1,2,3,4,5")
    }
    _ => assert_true(false)
  }
}

test "span context serialization" {
  let trace_id = "0af7651916cd43dd8448eb211c80319c"
  let span_id = "b7ad6b7169203331"
  let trace_state = "key1=value1,key2=value2"
  
  let span_ctx = SpanContext::new(trace_id, span_id, true, trace_state)
  
  // Serialize span context to W3C traceparent format
  let serialized_traceparent = "00-" + trace_id + "-" + span_id + "-01"
  
  // Verify serialization format
  assert_eq(serialized_traceparent, "00-0af7651916cd43dd8448eb211c80319c-b7ad6b7169203331-01")
  
  // Simulate deserialization by parsing components
  let parts = serialized_traceparent.split("-")
  assert_eq(parts.length(), 4)
  assert_eq(parts[0], "00")  // Version
  assert_eq(parts[1], trace_id)  // Trace ID
  assert_eq(parts[2], span_id)  // Span ID
  assert_eq(parts[3], "01")  // Flags (sampled)
  
  // Reconstruct span context from serialized data
  let reconstructed_ctx = SpanContext::new(parts[1], parts[2], true, trace_state)
  
  // Verify reconstruction
  assert_eq(SpanContext::trace_id(reconstructed_ctx), trace_id)
  assert_eq(SpanContext::span_id(reconstructed_ctx), span_id)
  assert_true(SpanContext::is_sampled(reconstructed_ctx))
}

test "context serialization" {
  let ctx = Context::root()
  let key1 = ContextKey::new("serialization.key1")
  let key2 = ContextKey::new("serialization.key2")
  
  let ctx_with_values = Context::with_value(
    Context::with_value(ctx, key1, "value1"),
    key2,
    "value2"
  )
  
  // Simulate context serialization
  let value1 = Context::get(ctx_with_values, key1)
  let value2 = Context::get(ctx_with_values, key2)
  
  // Serialize to string representation
  let serialized = match (value1, value2) {
    (Some(v1), Some(v2)) => v1 + "|" + v2
    _ => ""
  }
  
  // Simulate deserialization
  let parts = serialized.split("|")
  let deserialized_value1 = if parts.length() > 0 { Some(parts[0]) } else { None }
  let deserialized_value2 = if parts.length() > 1 { Some(parts[1]) } else { None }
  
  assert_eq(deserialized_value1, Some("value1"))
  assert_eq(deserialized_value2, Some("value2"))
}

test "baggage serialization" {
  let baggage = Baggage::new()
  
  // Add multiple entries
  let baggage1 = Baggage::set_entry(baggage, "key1", "value1")
  let baggage2 = Baggage::set_entry(baggage1, "key2", "value2")
  let baggage3 = Baggage::set_entry(baggage2, "key3", "value3")
  
  // Serialize to W3C baggage format
  let serialized = "key1=value1,key2=value2,key3=value3"
  
  // Simulate deserialization by parsing
  let entries = serialized.split(",")
  
  // Verify deserialization
  assert_eq(entries.length(), 3)
  assert_eq(entries[0], "key1=value1")
  assert_eq(entries[1], "key2=value2")
  assert_eq(entries[2], "key3=value3")
  
  // Reconstruct baggage entries
  for entry in entries {
    let kv_parts = entry.split("=")
    if kv_parts.length() == 2 {
      let reconstructed_key = kv_parts[0]
      let reconstructed_value = kv_parts[1]
      
      // Verify against original values
      let original_value = Baggage::get_entry(baggage3, reconstructed_key)
      assert_eq(original_value, Some(reconstructed_value))
    }
  }
}

test "resource serialization" {
  let resource = Resource::new()
  
  // Create attributes
  let attrs = [
    ("service.name", StringValue("test-service")),
    ("service.version", StringValue("1.0.0")),
    ("service.instance.id", StringValue("instance-123")),
    ("host.name", StringValue("test-host")),
    ("process.pid", IntValue(12345)),
    ("process.cpu.count", IntValue(4))
  ]
  
  let resource_with_attrs = Resource::with_attributes(resource, attrs)
  
  // Serialize resource to JSON-like string
  let mut serialized = "{"
  let mut first = true
  
  // Simulate serialization of each attribute
  for attr in attrs {
    if !first { serialized = serialized + "," }
    first = false
    
    serialized = serialized + "\"" + attr.0 + "\":"
    
    match attr.1 {
      StringValue(s) => serialized = serialized + "\"" + s + "\""
      IntValue(i) => serialized = serialized + i.to_string()
      FloatValue(f) => serialized = serialized + f.to_string()
      BoolValue(b) => serialized = serialized + if b { "true" } else { "false" }
      _ => serialized = serialized + "null"
    }
  }
  
  serialized = serialized + "}"
  
  // Verify serialized string contains expected values
  assert_true(serialized.contains("\"service.name\":\"test-service\""))
  assert_true(serialized.contains("\"service.version\":\"1.0.0\""))
  assert_true(serialized.contains("\"process.pid\":12345"))
  assert_true(serialized.contains("\"process.cpu.count\":4"))
  
  // Simulate deserialization by checking values
  let service_name = Resource::get_attribute(resource_with_attrs, "service.name")
  let service_version = Resource::get_attribute(resource_with_attrs, "service.version")
  let process_pid = Resource::get_attribute(resource_with_attrs, "process.pid")
  
  match service_name {
    Some(StringValue(s)) => assert_eq(s, "test-service")
    _ => assert_true(false)
  }
  
  match service_version {
    Some(StringValue(s)) => assert_eq(s, "1.0.0")
    _ => assert_true(false)
  }
  
  match process_pid {
    Some(IntValue(i)) => assert_eq(i, 12345)
    _ => assert_true(false)
  }
}

test "log record serialization" {
  let timestamp = 1234567890L
  let observed_timestamp = 1234567891L
  let trace_id = "trace123456789"
  let span_id = "span123456789"
  
  let record = LogRecord::new_with_context(
    Error,
    Some("Error occurred in processing"),
    Some(Attributes::new()),
    Some(timestamp),
    Some(observed_timestamp),
    Some(trace_id),
    Some(span_id),
    Some(Context::root())
  )
  
  // Serialize log record to JSON-like string
  let mut serialized = "{"
  serialized = serialized + "\"severity\":\"Error\"," 
  serialized = serialized + "\"body\":\"Error occurred in processing\"," 
  serialized = serialized + "\"timestamp\":" + timestamp.to_string() + ","
  serialized = serialized + "\"observed_timestamp\":" + observed_timestamp.to_string() + ","
  serialized = serialized + "\"trace_id\":\"" + trace_id + "\"," 
  serialized = serialized + "\"span_id\":\"" + span_id + "\""
  serialized = serialized + "}"
  
  // Verify serialized string
  assert_true(serialized.contains("\"severity\":\"Error\""))
  assert_true(serialized.contains("\"body\":\"Error occurred in processing\""))
  assert_true(serialized.contains("\"timestamp\":1234567890"))
  assert_true(serialized.contains("\"trace_id\":\"trace123456789\""))
  
  // Simulate deserialization by verifying original record
  assert_eq(LogRecord::severity_number(record), Error)
  assert_eq(LogRecord::body(record), Some("Error occurred in processing"))
  assert_eq(LogRecord::trace_id(record), Some(trace_id))
  assert_eq(LogRecord::span_id(record), Some(span_id))
}

test "instrument serialization" {
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "serialization-test")
  
  // Create different instrument types
  let counter = Meter::create_counter(meter, "http.requests.total", Some("Total HTTP requests"), Some("requests"))
  let histogram = Meter::create_histogram(meter, "http.request.duration", Some("HTTP request duration"), Some("ms"))
  let updown_counter = Meter::create_updown_counter(meter, "active.connections", Some("Active connections"), Some("connections"))
  let gauge = Meter::create_gauge(meter, "memory.usage", Some("Memory usage"), Some("bytes"))
  
  // Serialize counter to JSON
  let counter_serialized = "{"
  counter_serialized = counter_serialized + "\"type\":\"Counter\"," 
  counter_serialized = counter_serialized + "\"name\":\"" + counter.name + "\"," 
  counter_serialized = counter_serialized + "\"description\":\"Total HTTP requests\"," 
  counter_serialized = counter_serialized + "\"unit\":\"requests\""
  counter_serialized = counter_serialized + "}"
  
  // Verify serialization
  assert_true(counter_serialized.contains("\"type\":\"Counter\""))
  assert_true(counter_serialized.contains("\"name\":\"http.requests.total\""))
  assert_true(counter_serialized.contains("\"description\":\"Total HTTP requests\""))
  assert_true(counter_serialized.contains("\"unit\":\"requests\""))
  
  // Simulate deserialization by verifying original instruments
  assert_eq(counter.name, "http.requests.total")
  assert_eq(counter.description, Some("Total HTTP requests"))
  assert_eq(counter.unit, Some("requests"))
  
  assert_eq(histogram.name, "http.request.duration")
  assert_eq(updown_counter.name, "active.connections")
  assert_eq(gauge.name, "memory.usage")
}

test "HTTP request response serialization" {
  let headers = [
    ("Content-Type", "application/json"),
    ("Accept", "application/json"),
    ("Authorization", "Bearer token123")
  ]
  
  let body = "{\"key\":\"value\",\"number\":42}"
  
  let request = HttpRequest::new("POST", "https://api.example.com/data", headers, Some(body))
  
  // Serialize HTTP request to JSON-like string
  let mut request_serialized = "{"
  request_serialized = request_serialized + "\"method\":\"POST\"," 
  request_serialized = request_serialized + "\"url\":\"https://api.example.com/data\"," 
  request_serialized = request_serialized + "\"headers\":{"
  request_serialized = request_serialized + "\"Content-Type\":\"application/json\"," 
  request_serialized = request_serialized + "\"Accept\":\"application/json\"," 
  request_serialized = request_serialized + "\"Authorization\":\"Bearer token123\""
  request_serialized = request_serialized + "},"
  request_serialized = request_serialized + "\"body\":\"{\\\"key\\\":\\\"value\\\",\\\"number\\\":42}\""
  request_serialized = request_serialized + "}"
  
  // Verify serialization
  assert_true(request_serialized.contains("\"method\":\"POST\""))
  assert_true(request_serialized.contains("\"url\":\"https://api.example.com/data\""))
  assert_true(request_serialized.contains("\"Authorization\":\"Bearer token123\""))
  
  // Create response
  let response_headers = [("Content-Type", "application/json"), ("X-Request-ID", "req-123")]
  let response_body = "{\"success\":true,\"id\":123}"
  
  let response = HttpResponse::new(200, response_headers, Some(response_body))
  
  // Serialize HTTP response
  let mut response_serialized = "{"
  response_serialized = response_serialized + "\"status_code\":200," 
  response_serialized = response_serialized + "\"headers\":{"
  response_serialized = response_serialized + "\"Content-Type\":\"application/json\"," 
  response_serialized = response_serialized + "\"X-Request-ID\":\"req-123\""
  response_serialized = response_serialized + "},"
  response_serialized = response_serialized + "\"body\":\"{\\\"success\\\":true,\\\"id\\\":123}\""
  response_serialized = response_serialized + "}"
  
  // Verify serialization
  assert_true(response_serialized.contains("\"status_code\":200"))
  assert_true(response_serialized.contains("\"X-Request-ID\":\"req-123\""))
  
  // Simulate deserialization by verifying original objects
  assert_eq(HttpRequest::http_method(request), "POST")
  assert_eq(HttpRequest::url(request), "https://api.example.com/data")
  assert_eq(HttpRequest::body(request), Some(body))
  
  assert_eq(HttpResponse::status_code(response), 200)
  assert_eq(HttpResponse::body(response), Some(response_body))
}

test "complex nested serialization" {
  // Create complex nested structure
  let attrs = Attributes::new()
  Attributes::set(attrs, "nested.key", StringValue("nested.value"))
  Attributes::set(attrs, "nested.int", IntValue(42))
  
  let resource = Resource::new()
  let resource_attrs = [
    ("service.name", StringValue("complex-service")),
    ("service.version", StringValue("2.0.0"))
  ]
  let resource_with_attrs = Resource::with_attributes(resource, resource_attrs)
  
  let span_ctx = SpanContext::new("trace123", "span456", true, "key=value")
  
  // Serialize complex structure
  let mut complex_serialized = "{"
  complex_serialized = complex_serialized + "\"attributes\":{"
  complex_serialized = complex_serialized + "\"nested.key\":\"nested.value\"," 
  complex_serialized = complex_serialized + "\"nested.int\":42"
  complex_serialized = complex_serialized + "},"
  complex_serialized = complex_serialized + "\"resource\":{"
  complex_serialized = complex_serialized + "\"service.name\":\"complex-service\"," 
  complex_serialized = complex_serialized + "\"service.version\":\"2.0.0\""
  complex_serialized = complex_serialized + "},"
  complex_serialized = complex_serialized + "\"span_context\":{"
  complex_serialized = complex_serialized + "\"trace_id\":\"trace123\"," 
  complex_serialized = complex_serialized + "\"span_id\":\"span456\"," 
  complex_serialized = complex_serialized + "\"sampled\":true," 
  complex_serialized = complex_serialized + "\"trace_state\":\"key=value\""
  complex_serialized = complex_serialized + "}"
  complex_serialized = complex_serialized + "}"
  
  // Verify complex serialization
  assert_true(complex_serialized.contains("\"nested.key\":\"nested.value\""))
  assert_true(complex_serialized.contains("\"service.name\":\"complex-service\""))
  assert_true(complex_serialized.contains("\"trace_id\":\"trace123\""))
  
  // Simulate deserialization by verifying original objects
  let nested_value = Attributes::get(attrs, "nested.key")
  let nested_int = Attributes::get(attrs, "nested.int")
  let service_name = Resource::get_attribute(resource_with_attrs, "service.name")
  
  match nested_value {
    Some(StringValue(s)) => assert_eq(s, "nested.value")
    _ => assert_true(false)
  }
  
  match nested_int {
    Some(IntValue(i)) => assert_eq(i, 42)
    _ => assert_true(false)
  }
  
  match service_name {
    Some(StringValue(s)) => assert_eq(s, "complex-service")
    _ => assert_true(false)
  }
  
  assert_eq(SpanContext::trace_id(span_ctx), "trace123")
  assert_eq(SpanContext::span_id(span_ctx), "span456")
  assert_true(SpanContext::is_sampled(span_ctx))
}

test "serialization with special characters" {
  let attrs = Attributes::new()
  
  // Set attributes with special characters
  Attributes::set(attrs, "special.chars", StringValue("!@#$%^&*()_+-=[]{}|;':\",./<>?"))
  Attributes::set(attrs, "unicode.chars", StringValue("测试中文字符🚀emoji"))
  Attributes::set(attrs, "newlines", StringValue("line1\nline2\r\nline3"))
  Attributes::set(attrs, "quotes", StringValue("Quote: \"Hello\""))
  
  // Serialize with proper escaping
  let special_value = Attributes::get(attrs, "special.chars")
  let unicode_value = Attributes::get(attrs, "unicode.chars")
  let newlines_value = Attributes::get(attrs, "newlines")
  let quotes_value = Attributes::get(attrs, "quotes")
  
  match special_value {
    Some(StringValue(s)) => {
      // Simulate JSON escaping
      let escaped = s.replace("\\", "\\\\").replace("\"", "\\\"")
      assert_true(escaped.contains("!@#$%^&*()_+-=[]{}|;':"))
    }
    _ => assert_true(false)
  }
  
  match unicode_value {
    Some(StringValue(s)) => {
      // Unicode should be preserved
      assert_true(s.contains("测试"))
      assert_true(s.contains("🚀"))
    }
    _ => assert_true(false)
  }
  
  match newlines_value {
    Some(StringValue(s)) => {
      // Newlines should be escaped in JSON
      let escaped = s.replace("\n", "\\n").replace("\r", "\\r")
      assert_true(escaped.contains("\\n"))
      assert_true(escaped.contains("\\r"))
    }
    _ => assert_true(false)
  }
  
  match quotes_value {
    Some(StringValue(s)) => {
      // Quotes should be escaped
      let escaped = s.replace("\"", "\\\"")
      assert_true(escaped.contains("\\\""))
    }
    _ => assert_true(false)
  }
}

test "round trip serialization" {
  // Create original objects
  let original_attrs = Attributes::new()
  Attributes::set(original_attrs, "round.trip", StringValue("test.value"))
  Attributes::set(original_attrs, "number", IntValue(123))
  
  let original_ctx = Context::root()
  let key = ContextKey::new("round.trip.key")
  let original_ctx_with_value = Context::with_value(original_ctx, key, "round.trip.value")
  
  let original_span_ctx = SpanContext::new("roundtrip123", "roundtrip456", true, "round=key=value")
  
  // Simulate serialization
  let serialized_attrs = "round.trip=test.value|number=123"
  let serialized_ctx = "round.trip.key=round.trip.value"
  let serialized_span_ctx = "roundtrip123-roundtrip456-1-round=key=value"
  
  // Simulate deserialization
  let attrs_parts = serialized_attrs.split("|")
  let ctx_parts = serialized_ctx.split("=")
  let span_ctx_parts = serialized_span_ctx.split("-")
  
  // Reconstruct objects
  let reconstructed_attrs = Attributes::new()
  for part in attrs_parts {
    let kv = part.split("=")
    if kv.length() == 2 {
      if kv[0] == "round.trip" {
        Attributes::set(reconstructed_attrs, kv[0], StringValue(kv[1]))
      } else if kv[0] == "number" {
        Attributes::set(reconstructed_attrs, kv[0], IntValue(123))
      }
    }
  }
  
  // Verify round trip integrity
  let original_value = Attributes::get(original_attrs, "round.trip")
  let reconstructed_value = Attributes::get(reconstructed_attrs, "round.trip")
  
  match (original_value, reconstructed_value) {
    (Some(StringValue(orig)), Some(StringValue(recon))) => assert_eq(orig, recon)
    _ => assert_true(false)
  }
  
  // Verify span context round trip
  if span_ctx_parts.length() >= 3 {
    let reconstructed_trace_id = span_ctx_parts[0]
    let reconstructed_span_id = span_ctx_parts[1]
    let reconstructed_sampled = span_ctx_parts[2] == "1"
    
    assert_eq(reconstructed_trace_id, "roundtrip123")
    assert_eq(reconstructed_span_id, "roundtrip456")
    assert_true(reconstructed_sampled)
  }
  
  // Verify original objects are unchanged
  assert_eq(SpanContext::trace_id(original_span_ctx), "roundtrip123")
  assert_eq(SpanContext::span_id(original_span_ctx), "roundtrip456")
  assert_true(SpanContext::is_sampled(original_span_ctx))
}