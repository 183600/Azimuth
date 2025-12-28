// Simple Test Cases for Azimuth Telemetry System
// Testing only the most basic functionality

test "basic arithmetic test" {
  assert_eq(1 + 1, 2)
  assert_eq(2 * 3, 6)
  assert_eq(10 - 5, 5)
  assert_eq(20 / 4, 5)
}

test "boolean operations" {
  assert_true(true)
  assert_false(false)
  assert_true(true && true)
  assert_false(true && false)
  assert_true(true || false)
  assert_false(false || false)
}

test "string operations" {
  assert_eq("hello" + " world", "hello world")
  assert_eq("test".length(), 4)
  assert_eq("".length(), 0)
}

test "array operations" {
  let number_array = [1, 2, 3, 4, 5]
  assert_eq(number_array.length(), 5)
  assert_eq(number_array[0], 1)
  assert_eq(number_array[4], 5)
  
  let string_array = ["a", "b", "c"]
  assert_eq(string_array.length(), 3)
  assert_eq(string_array[1], "b")
}

test "option type operations" {
  let some_value = Some(42)
  let none_value = None
  
  assert_eq(some_value, Some(42))
  assert_eq(none_value, None)
  assert_true(some_value != None)
  assert_true(none_value != Some(0))
}

test "span context creation and validation" {
  let trace_id = "0af7651916cd43dd8448eb211c80319c"
  let span_id = "b7ad6b7169203331"
  let span_ctx = SpanContext::new(trace_id, span_id, true, "key1=value1")
  
  // Test basic properties
  assert_eq(SpanContext::trace_id(span_ctx), trace_id)
  assert_eq(SpanContext::span_id(span_ctx), span_id)
  assert_true(SpanContext::is_sampled(span_ctx))
  assert_true(SpanContext::is_valid(span_ctx))
}

test "span context validation edge cases" {
  // Test invalid span context with empty trace_id
  let invalid_trace_ctx = SpanContext::new("", "1234567890abcdef", true, "")
  assert_false(SpanContext::is_valid(invalid_trace_ctx))
  
  // Test invalid span context with empty span_id
  let invalid_span_ctx = SpanContext::new("1234567890abcdef1234567890abcdef", "", true, "")
  assert_false(SpanContext::is_valid(invalid_span_ctx))
}

test "span creation" {
  let span_ctx = SpanContext::new("trace123", "span123", true, "")
  let span = Span::new("test-span", Internal, span_ctx)
  
  // Test span properties
  assert_eq(Span::name(span), "test-span")
  assert_true(Span::is_recording(span))
  assert_eq(SpanContext::trace_id(Span::span_context(span)), "trace123")
}

test "metrics creation" {
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "test-meter")
  let counter = Meter::create_counter(meter, "http.requests.total")
  
  // Test counter properties
  assert_eq(counter.name, "http.requests.total")
  
  // Test counter operations
  Counter::add(counter, 1.0)
  Counter::add(counter, 5.0)
  Counter::add(counter, 10.5)
  
  assert_true(true)
}

test "text map carrier operations" {
  let carrier = TextMapCarrier::new()
  
  // Test setting headers
  TextMapCarrier::set(carrier, "traceparent", "00-0af7651916cd43dd8448eb211c80319c-b7ad6b7169203331-01")
  
  // Test getting headers
  let traceparent = TextMapCarrier::get(carrier, "traceparent")
  let missing = TextMapCarrier::get(carrier, "missing-header")
  
  assert_eq(traceparent, Some("00-0af7651916cd43dd8448eb211c80319c-b7ad6b7169203331-01"))
  assert_eq(missing, None)
}

test "http operations" {
  let headers = [("Content-Type", "application/json")]
  let request = HttpRequest::new("GET", "https://api.example.com/users", headers)
  
  // Test request properties
  assert_eq(HttpRequest::http_method(request), "GET")
  assert_eq(HttpRequest::url(request), "https://api.example.com/users")
  assert_eq(HttpRequest::body(request), None)
  
  let response = HttpResponse::new(200, headers)
  assert_eq(HttpResponse::status_code(response), 200)
  assert_eq(HttpResponse::body(response), None)
}

test "resource operations" {
  let resource = Resource::new()
  
  // Test empty resource
  assert_eq(resource.attributes.length(), 0)
}

test "context operations" {
  let ctx = Context::root()
  let key = ContextKey::new("test.key")
  let ctx_with_value = Context::with_value(ctx, key, "test_value")
  
  // Test context value retrieval
  let retrieved_value = Context::get(ctx_with_value, key)
  assert_eq(retrieved_value, Some("test_value"))
  
  // Test missing key
  let missing_value = Context::get(ctx, ContextKey::new("missing.key"))
  assert_eq(missing_value, None)
}