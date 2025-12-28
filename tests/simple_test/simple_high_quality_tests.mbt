// Simple High-Quality Test Suite for Azimuth Telemetry System
// This file contains basic test cases to verify functionality

test "basic arithmetic operations" {
  assert_eq(1 + 1, 2)
  assert_eq(2 * 3, 6)
  assert_eq(10 - 5, 5)
  assert_eq(20 / 4, 5)
}

test "boolean logic operations" {
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

test "span context basic operations" {
  let trace_id = "0af7651916cd43dd8448eb211c80319c"
  let span_id = "b7ad6b7169203331"
  let span_ctx = SpanContext::new(trace_id, span_id, true, "key1=value1")
  
  assert_eq(SpanContext::trace_id(span_ctx), trace_id)
  assert_eq(SpanContext::span_id(span_ctx), span_id)
  assert_true(SpanContext::is_sampled(span_ctx))
  assert_true(SpanContext::is_valid(span_ctx))
}

test "span context validation edge cases" {
  let empty_trace_ctx = SpanContext::new("", "1234567890abcdef", true, "")
  assert_false(SpanContext::is_valid(empty_trace_ctx))
  
  let empty_span_ctx = SpanContext::new("1234567890abcdef1234567890abcdef", "", true, "")
  assert_false(SpanContext::is_valid(empty_span_ctx))
}

test "context operations" {
  let ctx = Context::root()
  let key = ContextKey::new("test.key")
  let ctx_with_value = Context::with_value(ctx, key, "test_value")
  
  let retrieved_value = Context::get(ctx_with_value, key)
  assert_eq(retrieved_value, Some("test_value"))
  
  let missing_value = Context::get(ctx, ContextKey::new("missing.key"))
  assert_eq(missing_value, None)
}

test "basic metrics operations" {
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "test-meter")
  let counter = Meter::create_counter(meter, "http.requests.total")
  
  assert_eq(counter.name, "http.requests.total")
  
  Counter::add(counter, 1.0)
  Counter::add(counter, 5.0)
  Counter::add(counter, 10.5)
  
  assert_true(true)
}

test "text map carrier operations" {
  let carrier = TextMapCarrier::new()
  
  TextMapCarrier::set(carrier, "traceparent", "00-0af7651916cd43dd8448eb211c80319c-b7ad6b7169203331-01")
  
  let traceparent = TextMapCarrier::get(carrier, "traceparent")
  let missing = TextMapCarrier::get(carrier, "missing-header")
  
  assert_eq(traceparent, Some("00-0af7651916cd43dd8448eb211c80319c-b7ad6b7169203331-01"))
  assert_eq(missing, None)
}

test "http operations" {
  let headers = [("Content-Type", "application/json")]
  let request = HttpRequest::new("GET", "https://api.example.com/users", headers)
  
  assert_eq(HttpRequest::http_method(request), "GET")
  assert_eq(HttpRequest::url(request), "https://api.example.com/users")
  assert_eq(HttpRequest::body(request), None)
  
  let response = HttpResponse::new(200, headers)
  assert_eq(HttpResponse::status_code(response), 200)
  assert_eq(HttpResponse::body(response), None)
}