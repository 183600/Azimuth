// Basic Test Suite for Azimuth Telemetry System
// This file contains very basic test cases to verify the system works

test "basic arithmetic" {
  assert_eq(1 + 1, 2)
  assert_eq(2 * 3, 6)
}

test "boolean logic" {
  assert_true(true)
  assert_false(false)
}

test "string operations" {
  assert_eq("hello" + " world", "hello world")
}

test "option types" {
  let some_value = Some(42)
  let none_value = None
  assert_eq(some_value, Some(42))
  assert_eq(none_value, None)
}

test "span context creation" {
  let trace_id = "0af7651916cd43dd8448eb211c80319c"
  let span_id = "b7ad6b7169203331"
  let span_ctx = SpanContext::new(trace_id, span_id, true, "key1=value1")
  
  assert_eq(SpanContext::trace_id(span_ctx), trace_id)
  assert_eq(SpanContext::span_id(span_ctx), span_id)
  assert_true(SpanContext::is_sampled(span_ctx))
  assert_true(SpanContext::is_valid(span_ctx))
}

test "context operations" {
  let ctx = Context::root()
  let key = ContextKey::new("test.key")
  let ctx_with_value = Context::with_value(ctx, key, "test_value")
  
  let retrieved_value = Context::get(ctx_with_value, key)
  assert_eq(retrieved_value, Some("test_value"))
}

test "metrics operations" {
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "test-meter")
  let counter = Meter::create_counter(meter, "http.requests.total")
  
  assert_eq(counter.name, "http.requests.total")
  
  Counter::add(counter, 1.0)
  Counter::add(counter, 5.0)
  
  assert_true(true)
}

test "log record creation" {
  let record = LogRecord::new(Info, "Test log message")
  
  assert_eq(LogRecord::severity_number(record), Info)
  assert_eq(LogRecord::body(record), Some("Test log message"))
}

test "text map carrier" {
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

test "clock and random" {
  let clock = Clock::system()
  let timestamp = Clock::now_unix_nanos(clock)
  
  assert_true(timestamp > 0L)
  
  let random = Random::system()
  let random_u64 = Random::next_u64(random)
  
  assert_eq(random_u64, 12345UL)
}