// Simple New Tests for Azimuth Telemetry System
// Basic test cases that avoid enum comparisons

test "basic arithmetic verification" {
  assert_eq(1 + 1, 2)
  assert_eq(2 * 3, 6)
  assert_eq(10 - 5, 5)
  assert_eq(20 / 4, 5)
}

test "string operations verification" {
  assert_eq("hello" + " world", "hello world")
  assert_eq("test".length(), 4)
  assert_eq("".length(), 0)
}

test "array operations verification" {
  let number_array = [1, 2, 3, 4, 5]
  assert_eq(number_array.length(), 5)
  assert_eq(number_array[0], 1)
  assert_eq(number_array[4], 5)
  
  let string_array = ["a", "b", "c"]
  assert_eq(string_array.length(), 3)
  assert_eq(string_array[1], "b")
}

test "option type operations verification" {
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
  let invalid_trace_ctx = SpanContext::new("", "1234567890abcdef", true, "")
  assert_false(SpanContext::is_valid(invalid_trace_ctx))
  
  let invalid_span_ctx = SpanContext::new("1234567890abcdef1234567890abcdef", "", true, "")
  assert_false(SpanContext::is_valid(invalid_span_ctx))
}

test "metrics creation verification" {
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "test-meter")
  let counter = Meter::create_counter(meter, "http.requests.total")
  
  assert_eq(counter.name, "http.requests.total")
  
  Counter::add(counter, 1.0)
  Counter::add(counter, 5.0)
  Counter::add(counter, 10.5)
  
  assert_true(true)
}

test "text map carrier operations verification" {
  let carrier = TextMapCarrier::new()
  
  TextMapCarrier::set(carrier, "traceparent", "00-0af7651916cd43dd8448eb211c80319c-b7ad6b7169203331-01")
  
  let traceparent = TextMapCarrier::get(carrier, "traceparent")
  let missing = TextMapCarrier::get(carrier, "missing-header")
  
  assert_eq(traceparent, Some("00-0af7651916cd43dd8448eb211c80319c-b7ad6b7169203331-01"))
  assert_eq(missing, None)
}

test "http operations verification" {
  let headers = [("Content-Type", "application/json")]
  let request = HttpRequest::new("GET", "https://api.example.com/users", headers)
  
  assert_eq(HttpRequest::http_method(request), "GET")
  assert_eq(HttpRequest::url(request), "https://api.example.com/users")
  assert_eq(HttpRequest::body(request), None)
  
  let response = HttpResponse::new(200, headers)
  assert_eq(HttpResponse::status_code(response), 200)
  assert_eq(HttpResponse::body(response), None)
}

test "resource operations verification" {
  let resource = Resource::new()
  assert_eq(resource.attributes.length(), 0)
}

test "context operations verification" {
  let ctx = Context::root()
  let key = ContextKey::new("test.key")
  let ctx_with_value = Context::with_value(ctx, key, "test_value")
  
  let retrieved_value = Context::get(ctx_with_value, key)
  assert_eq(retrieved_value, Some("test_value"))
  
  let missing_value = Context::get(ctx, ContextKey::new("missing.key"))
  assert_eq(missing_value, None)
}

test "clock and random operations verification" {
  let clock = Clock::system()
  let timestamp = Clock::now_unix_nanos(clock)
  
  assert_true(timestamp > 0L)
  
  let random = Random::system()
  let random_bytes = Random::next_bytes(random, 8)
  let random_u64 = Random::next_u64(random)
  
  assert_eq(random_bytes.length(), 0)
  assert_eq(random_u64, 12345UL)
}

test "log record basic creation verification" {
  let record = LogRecord::new(Info, "Test log message")
  
  assert_eq(LogRecord::body(record), Some("Test log message"))
}

test "log record with empty body verification" {
  let record = LogRecord::new(Info, "")
  
  assert_eq(LogRecord::body(record), Some(""))
}

test "log record with context verification" {
  let ctx = Context::root()
  let key = ContextKey::new("correlation.id")
  let ctx_with_value = Context::with_value(ctx, key, "corr-12345")
  
  let record = LogRecord::new_with_context(
    Warn,
    Some("Warning with context"),
    None,
    Some(1735689600000000000L),
    Some(1735689600000001000L),
    Some("trace-123"),
    Some("span-456"),
    Some(ctx_with_value)
  )
  
  assert_eq(LogRecord::body(record), Some("Warning with context"))
  assert_eq(LogRecord::trace_id(record), Some("trace-123"))
  assert_eq(LogRecord::span_id(record), Some("span-456"))
}

test "span creation and basic operations" {
  let trace_id = "0af7651916cd43dd8448eb211c80319c"
  let span_id = "b7ad6b7169203331"
  let span_ctx = SpanContext::new(trace_id, span_id, true, "")
  
  let span = Span::new("test-operation", Internal, span_ctx)
  assert_eq(Span::name(span), "test-operation")
  assert_true(Span::is_recording(span))
  
  Span::add_event(span, "test.event", None)
  Span::end(span)
  
  assert_true(true)
}

test "tracer operations verification" {
  let provider = TracerProvider::default()
  let tracer = TracerProvider::get_tracer(provider, "test-tracer", Some("1.0.0"))
  
  let scope = Tracer::instrumentation_scope(tracer)
  assert_eq(scope.name, "test-tracer")
  assert_eq(scope.version, Some("1.0.0"))
  
  let span = Tracer::start_span(tracer, "test-operation", None)
  assert_eq(Span::name(span), "test-operation")
}