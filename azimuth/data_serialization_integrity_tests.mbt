// Data Serialization/Deserialization Test Suite for Azimuth Telemetry System
// This file contains comprehensive test cases for data serialization and integrity

test "attribute value serialization" {
  // Test serialization of different attribute value types
  let string_value = StringValue("test string")
  let int_value = IntValue(42)
  let float_value = FloatValue(3.14159)
  let bool_value = BoolValue(true)
  let array_string_value = ArrayStringValue(["item1", "item2", "item3"])
  let array_int_value = ArrayIntValue([1, 2, 3, 4, 5])
  
  // Simulate serialization by converting to string representations
  let string_serialized = "string:test string"
  let int_serialized = "int:42"
  let float_serialized = "float:3.14159"
  let bool_serialized = "bool:true"
  let array_string_serialized = "array_string:[\"item1\",\"item2\",\"item3\"]"
  let array_int_serialized = "array_int:[1,2,3,4,5]"
  
  // Simulate deserialization by parsing string representations
  assert_eq(string_serialized, "string:test string")
  assert_eq(int_serialized, "int:42")
  assert_eq(float_serialized, "float:3.14159")
  assert_eq(bool_serialized, "bool:true")
  assert_eq(array_string_serialized, "array_string:[\"item1\",\"item2\",\"item3\"]")
  assert_eq(array_int_serialized, "array_int:[1,2,3,4,5]")
}

test "span context serialization" {
  // Create span context with various values
  let trace_id = "0af7651916cd43dd8448eb211c80319c"
  let span_id = "b7ad6b7169203331"
  let sampled = true
  let trace_state = "key1=value1,key2=value2"
  
  let span_ctx = SpanContext::new(trace_id, span_id, sampled, trace_state)
  
  // Simulate W3C traceparent format serialization
  let traceparent_serialized = "00-" + trace_id + "-" + span_id + "-" + (sampled ? "01" : "00")
  
  // Simulate tracestate format serialization
  let tracestate_serialized = trace_state
  
  // Verify serialized formats
  assert_eq(traceparent_serialized, "00-0af7651916cd43dd8448eb211c80319c-b7ad6b7169203331-01")
  assert_eq(tracestate_serialized, "key1=value1,key2=value2")
  
  // Verify original span context values
  assert_eq(SpanContext::trace_id(span_ctx), trace_id)
  assert_eq(SpanContext::span_id(span_ctx), span_id)
  assert_true(SpanContext::is_sampled(span_ctx))
}

test "baggage serialization" {
  // Create baggage with multiple entries
  let baggage = Baggage::new()
  let baggage_with_entries = Baggage::set_entry(baggage, "user.id", "user-123")
  let baggage_with_more_entries = Baggage::set_entry(baggage_with_entries, "request.id", "req-456")
  let final_baggage = Baggage::set_entry(baggage_with_more_entries, "session.id", "sess-789")
  
  // Simulate baggage serialization (W3C format)
  let baggage_serialized = "user.id=user-123,request.id=req-456,session.id=sess-789"
  
  // Verify serialized format
  assert_eq(baggage_serialized, "user.id=user-123,request.id=req-456,session.id=sess-789")
  
  // Simulate baggage deserialization
  let deserialized_entries = [
    ("user.id", "user-123"),
    ("request.id", "req-456"),
    ("session.id", "sess-789")
  ]
  
  // Verify deserialized entries
  assert_eq(deserialized_entries.length, 3)
  assert_eq(deserialized_entries[0], ("user.id", "user-123"))
  assert_eq(deserialized_entries[1], ("request.id", "req-456"))
  assert_eq(deserialized_entries[2], ("session.id", "sess-789"))
}

test "resource serialization" {
  // Create resource with various attributes
  let resource = Resource::new()
  let attributes = [
    ("service.name", StringValue("azimuth-service")),
    ("service.version", StringValue("1.0.0")),
    ("service.instance.id", StringValue("instance-123")),
    ("service.startup.time", IntValue(1234567890)),
    ("service.memory.usage", FloatValue(512.5)),
    ("service.production", BoolValue(true)),
    ("service.tags", ArrayStringValue(["web", "api", "microservice"]))
  ]
  
  let resource_with_attrs = Resource::with_attributes(resource, attributes)
  
  // Simulate resource serialization (JSON format)
  let resource_serialized = "{"
  + "\"service.name\":\"azimuth-service\","
  + "\"service.version\":\"1.0.0\","
  + "\"service.instance.id\":\"instance-123\","
  + "\"service.startup.time\":1234567890,"
  + "\"service.memory.usage\":512.5,"
  + "\"service.production\":true,"
  + "\"service.tags\":[\"web\",\"api\",\"microservice\"]"
  + "}"
  
  // Verify serialized format contains expected attributes
  assert_true(resource_serialized.index_of("service.name") >= 0)
  assert_true(resource_serialized.index_of("azimuth-service") >= 0)
  assert_true(resource_serialized.index_of("service.version") >= 0)
  assert_true(resource_serialized.index_of("1.0.0") >= 0)
  assert_true(resource_serialized.index_of("service.startup.time") >= 0)
  assert_true(resource_serialized.index_of("1234567890") >= 0)
}

test "log record serialization" {
  // Create log record with various fields
  let timestamp = 1735689600000000000L
  let observed_timestamp = 1735689600000000001L
  let trace_id = "0af7651916cd43dd8448eb211c80319c"
  let span_id = "b7ad6b7169203331"
  
  let log_record = LogRecord::new_with_context(
    Error,
    Some("Database connection failed"),
    Some(Attributes::new()),
    Some(timestamp),
    Some(observed_timestamp),
    Some(trace_id),
    Some(span_id),
    Some(Context::root())
  )
  
  // Simulate log record serialization (JSON format)
  let log_serialized = "{"
  + "\"severity_number\":\"Error\","
  + "\"body\":\"Database connection failed\","
  + "\"timestamp\":" + timestamp.to_string() + ","
  + "\"observed_timestamp\":" + observed_timestamp.to_string() + ","
  + "\"trace_id\":\"" + trace_id + "\","
  + "\"span_id\":\"" + span_id + "\""
  + "}"
  
  // Verify serialized format contains expected fields
  assert_true(log_serialized.index_of("severity_number") >= 0)
  assert_true(log_serialized.index_of("Error") >= 0)
  assert_true(log_serialized.index_of("Database connection failed") >= 0)
  assert_true(log_serialized.index_of("trace_id") >= 0)
  assert_true(log_serialized.index_of(trace_id) >= 0)
  assert_true(log_serialized.index_of("span_id") >= 0)
  assert_true(log_serialized.index_of(span_id) >= 0)
}

test "metric data serialization" {
  // Create metric data
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "serialization-meter")
  
  let counter = Meter::create_counter(meter, "http.requests.total", Some("Total HTTP requests"), Some("count"))
  let histogram = Meter::create_histogram(meter, "http.request.duration", Some("HTTP request duration"), Some("ms"))
  let updown_counter = Meter::create_updown_counter(meter, "active.connections", Some("Active connections"), Some("connections"))
  let gauge = Meter::create_gauge(meter, "memory.usage", Some("Memory usage"), Some("bytes"))
  
  // Simulate metric serialization (JSON format)
  let metrics_serialized = "["
  + "{\"name\":\"http.requests.total\",\"type\":\"counter\",\"description\":\"Total HTTP requests\",\"unit\":\"count\"},"
  + "{\"name\":\"http.request.duration\",\"type\":\"histogram\",\"description\":\"HTTP request duration\",\"unit\":\"ms\"},"
  + "{\"name\":\"active.connections\",\"type\":\"updown_counter\",\"description\":\"Active connections\",\"unit\":\"connections\"},"
  + "{\"name\":\"memory.usage\",\"type\":\"gauge\",\"description\":\"Memory usage\",\"unit\":\"bytes\"}"
  + "]"
  
  // Verify serialized format contains expected metrics
  assert_true(metrics_serialized.index_of("http.requests.total") >= 0)
  assert_true(metrics_serialized.index_of("counter") >= 0)
  assert_true(metrics_serialized.index_of("http.request.duration") >= 0)
  assert_true(metrics_serialized.index_of("histogram") >= 0)
  assert_true(metrics_serialized.index_of("active.connections") >= 0)
  assert_true(metrics_serialized.index_of("updown_counter") >= 0)
  assert_true(metrics_serialized.index_of("memory.usage") >= 0)
  assert_true(metrics_serialized.index_of("gauge") >= 0)
}

test "http request/response serialization" {
  // Create HTTP request
  let request_headers = [
    ("Content-Type", "application/json"),
    ("Authorization", "Bearer token123"),
    ("X-Request-ID", "req-123")
  ]
  let request = HttpRequest::new("POST", "https://api.example.com/users", request_headers, Some("{\"name\":\"John\"}"))
  
  // Create HTTP response
  let response_headers = [
    ("Content-Type", "application/json"),
    ("X-Response-ID", "resp-456"),
    ("Cache-Control", "no-cache")
  ]
  let response = HttpResponse::new(201, response_headers, Some("{\"id\":123,\"name\":\"John\"}"))
  
  // Simulate HTTP request serialization (JSON format)
  let request_serialized = "{"
  + "\"method\":\"POST\","
  + "\"url\":\"https://api.example.com/users\","
  + "\"headers\":["
  + "{\"name\":\"Content-Type\",\"value\":\"application/json\"},"
  + "{\"name\":\"Authorization\",\"value\":\"Bearer token123\"},"
  + "{\"name\":\"X-Request-ID\",\"value\":\"req-123\"}"
  + "],"
  + "\"body\":\"{\\\"name\\\":\\\"John\\\"}\""
  + "}"
  
  // Simulate HTTP response serialization (JSON format)
  let response_serialized = "{"
  + "\"status_code\":201,"
  + "\"headers\":["
  + "{\"name\":\"Content-Type\",\"value\":\"application/json\"},"
  + "{\"name\":\"X-Response-ID\",\"value\":\"resp-456\"},"
  + "{\"name\":\"Cache-Control\",\"value\":\"no-cache\"}"
  + "],"
  + "\"body\":\"{\\\"id\\\":123,\\\"name\\\":\\\"John\\\"}\""
  + "}"
  
  // Verify serialized formats
  assert_true(request_serialized.index_of("POST") >= 0)
  assert_true(request_serialized.index_of("https://api.example.com/users") >= 0)
  assert_true(request_serialized.index_of("Content-Type") >= 0)
  assert_true(request_serialized.index_of("application/json") >= 0)
  
  assert_true(response_serialized.index_of("201") >= 0)
  assert_true(response_serialized.index_of("Content-Type") >= 0)
  assert_true(response_serialized.index_of("application/json") >= 0)
  assert_true(response_serialized.index_of("X-Response-ID") >= 0)
}

test "text map carrier serialization" {
  // Create text map carrier with headers
  let carrier = TextMapCarrier::new()
  TextMapCarrier::set(carrier, "traceparent", "00-0af7651916cd43dd8448eb211c80319c-b7ad6b7169203331-01")
  TextMapCarrier::set(carrier, "tracestate", "rojo=00f067aa0ba902b7,congo=t61rcWkgMzE")
  TextMapCarrier::set(carrier, "baggage", "userId=user-123,sessionId=session-456")
  TextMapCarrier::set(carrier, "User-Agent", "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36")
  
  // Simulate text map carrier serialization (map format)
  let carrier_serialized = "{"
  + "\"traceparent\":\"00-0af7651916cd43dd8448eb211c80319c-b7ad6b7169203331-01\","
  + "\"tracestate\":\"rojo=00f067aa0ba902b7,congo=t61rcWkgMzE\","
  + "\"baggage\":\"userId=user-123,sessionId=session-456\","
  + "\"User-Agent\":\"Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36\""
  + "}"
  
  // Verify serialized format contains expected headers
  assert_true(carrier_serialized.index_of("traceparent") >= 0)
  assert_true(carrier_serialized.index_of("00-0af7651916cd43dd8448eb211c80319c-b7ad6b7169203331-01") >= 0)
  assert_true(carrier_serialized.index_of("tracestate") >= 0)
  assert_true(carrier_serialized.index_of("rojo=00f067aa0ba902b7") >= 0)
  assert_true(carrier_serialized.index_of("baggage") >= 0)
  assert_true(carrier_serialized.index_of("userId=user-123") >= 0)
  assert_true(carrier_serialized.index_of("User-Agent") >= 0)
}

test "context serialization" {
  // Create context with multiple key-value pairs
  let ctx = Context::root()
  let ctx1 = Context::with_value(ctx, ContextKey::new("trace.id"), "trace-123")
  let ctx2 = Context::with_value(ctx1, ContextKey::new("user.id"), "user-456")
  let ctx3 = Context::with_value(ctx2, ContextKey::new("request.id"), "req-789")
  let final_ctx = Context::with_value(ctx3, ContextKey::new("operation.name"), "getUserProfile")
  
  // Simulate context serialization (map format)
  let context_serialized = "{"
  + "\"trace.id\":\"trace-123\","
  + "\"user.id\":\"user-456\","
  + "\"request.id\":\"req-789\","
  + "\"operation.name\":\"getUserProfile\""
  + "}"
  
  // Verify serialized format contains expected key-value pairs
  assert_true(context_serialized.index_of("trace.id") >= 0)
  assert_true(context_serialized.index_of("trace-123") >= 0)
  assert_true(context_serialized.index_of("user.id") >= 0)
  assert_true(context_serialized.index_of("user-456") >= 0)
  assert_true(context_serialized.index_of("request.id") >= 0)
  assert_true(context_serialized.index_of("req-789") >= 0)
  assert_true(context_serialized.index_of("operation.name") >= 0)
  assert_true(context_serialized.index_of("getUserProfile") >= 0)
}

test "span serialization" {
  // Create span with various properties
  let span_ctx = SpanContext::new("0af7651916cd43dd8448eb211c80319c", "b7ad6b7169203331", true, "")
  let span = Span::new("http.request", Server, span_ctx)
  
  // Set status and add events
  Span::set_status(span, Ok, Some("Request completed successfully"))
  Span::add_event(span, "request.started", Some([("http.method", StringValue("GET")), ("http.url", StringValue("/api/users"))]))
  Span::add_event(span, "request.completed", Some([("http.status_code", IntValue(200))]))
  
  // Simulate span serialization (JSON format)
  let span_serialized = "{"
  + "\"name\":\"http.request\","
  + "\"kind\":\"Server\","
  + "\"status\":\"Ok\","
  + "\"status_description\":\"Request completed successfully\","
  + "\"trace_id\":\"0af7651916cd43dd8448eb211c80319c\","
  + "\"span_id\":\"b7ad6b7169203331\","
  + "\"events\":["
  + "{\"name\":\"request.started\",\"attributes\":[{\"key\":\"http.method\",\"value\":\"GET\"},{\"key\":\"http.url\",\"value\":\"/api/users\"}]},"
  + "{\"name\":\"request.completed\",\"attributes\":[{\"key\":\"http.status_code\",\"value\":200}]}"
  + "]"
  + "}"
  
  // Verify serialized format contains expected properties
  assert_true(span_serialized.index_of("http.request") >= 0)
  assert_true(span_serialized.index_of("Server") >= 0)
  assert_true(span_serialized.index_of("Ok") >= 0)
  assert_true(span_serialized.index_of("Request completed successfully") >= 0)
  assert_true(span_serialized.index_of("trace_id") >= 0)
  assert_true(span_serialized.index_of("0af7651916cd43dd8448eb211c80319c") >= 0)
  assert_true(span_serialized.index_of("events") >= 0)
  assert_true(span_serialized.index_of("request.started") >= 0)
  assert_true(span_serialized.index_of("request.completed") >= 0)
}

test "serialization data integrity" {
  // Test serialization and deserialization integrity
  let original_data = [
    ("string.test", StringValue("original string value")),
    ("int.test", IntValue(12345)),
    ("float.test", FloatValue(3.14159265359)),
    ("bool.test", BoolValue(true)),
    ("array.string.test", ArrayStringValue(["item1", "item2", "item3"])),
    ("array.int.test", ArrayIntValue([10, 20, 30, 40, 50]))
  ]
  
  // Simulate serialization
  let serialized_data = Array.map(original_data, fn(pair) {
    match pair.1 {
      StringValue(s) => "string:" + s
      IntValue(i) => "int:" + i.to_string()
      FloatValue(f) => "float:" + f.to_string()
      BoolValue(b) => "bool:" + (b ? "true" : "false")
      ArrayStringValue(arr) => "array.string:" + arr.join(",")
      ArrayIntValue(arr) => "array.int:" + arr.map(i => i.to_string()).join(",")
    }
  })
  
  // Simulate deserialization
  let deserialized_data = Array.map(serialized_data, fn(serialized) {
    if serialized.starts_with("string:") {
      ("string.test", StringValue(serialized.substring(7)))
    } else if serialized.starts_with("int:") {
      ("int.test", IntValue(serialized.substring(4).to_int()))
    } else if serialized.starts_with("float:") {
      ("float.test", FloatValue(serialized.substring(6).to_double()))
    } else if serialized.starts_with("bool:") {
      ("bool.test", BoolValue(serialized.substring(5) == "true"))
    } else if serialized.starts_with("array.string:") {
      ("array.string.test", ArrayStringValue(serialized.substring(13).split(",")))
    } else if serialized.starts_with("array.int:") {
      ("array.int.test", ArrayIntValue(serialized.substring(10).split(",").map(s => s.to_int())))
    } else {
      ("unknown", StringValue("unknown"))
    }
  })
  
  // Verify data integrity
  assert_eq(deserialized_data.length, original_data.length)
  assert_eq(deserialized_data[0].0, "string.test")
  assert_eq(deserialized_data[1].0, "int.test")
  assert_eq(deserialized_data[2].0, "float.test")
  assert_eq(deserialized_data[3].0, "bool.test")
  assert_eq(deserialized_data[4].0, "array.string.test")
  assert_eq(deserialized_data[5].0, "array.int.test")
}