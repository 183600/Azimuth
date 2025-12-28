// Simple MoonBit Test Suite for Azimuth Telemetry System
// This file contains 5 basic test cases covering core functionality

test "attributes basic operations" {
  let attrs = Attributes::new()
  
  Attributes::set(attrs, "test.key", StringValue("test_value"))
  let result = Attributes::get(attrs, "test.key")
  
  assert_eq(result, Some(StringValue("test_value")))
}

test "span context validation" {
  let trace_id = "0af7651916cd43dd8448eb211c80319c"
  let span_id = "b7ad6b7169203331"
  let span_ctx = SpanContext::new(trace_id, span_id, true, "")
  
  assert_eq(SpanContext::trace_id(span_ctx), trace_id)
  assert_eq(SpanContext::span_id(span_ctx), span_id)
  assert_true(SpanContext::is_sampled(span_ctx))
  assert_true(SpanContext::is_valid(span_ctx))
}

test "context value operations" {
  let ctx = Context::root()
  let key = ContextKey::new("test.key")
  let ctx_with_value = Context::with_value(ctx, key, "test_value")
  
  let retrieved_value = Context::get(ctx_with_value, key)
  assert_eq(retrieved_value, Some("test_value"))
}

test "metrics counter operations" {
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "test-meter")
  let counter = Meter::create_counter(meter, "test.counter")
  
  assert_eq(counter.name, "test.counter")
  
  Counter::add(counter, 1.0)
  Counter::add(counter, 5.0)
  
  assert_true(true)
}

test "log record creation" {
  let record = LogRecord::new(Info, "Test log message")
  
  assert_eq(LogRecord::severity_number(record), Info)
  assert_eq(LogRecord::body(record), Some("Test log message"))
}