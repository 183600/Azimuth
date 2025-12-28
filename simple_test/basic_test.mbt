// Basic Test for Azimuth Telemetry System

test "simple arithmetic test" {
  assert_eq(1 + 1, 2)
  assert_eq(2 * 3, 6)
}

test "boolean logic test" {
  assert_true(true)
  assert_false(false)
}

test "string operations test" {
  assert_eq("hello" + " world", "hello world")
}

test "option types test" {
  let some_value = Some(42)
  let none_value = None
  assert_eq(some_value, Some(42))
  assert_eq(none_value, None)
}

test "span context creation test" {
  let trace_id = "0af7651916cd43dd8448eb211c80319c"
  let span_id = "b7ad6b7169203331"
  let span_ctx = SpanContext::new(trace_id, span_id, true, "key1=value1")
  
  assert_eq(SpanContext::trace_id(span_ctx), trace_id)
  assert_eq(SpanContext::span_id(span_ctx), span_id)
  assert_true(SpanContext::is_sampled(span_ctx))
  assert_true(SpanContext::is_valid(span_ctx))
}

test "context operations test" {
  let ctx = Context::root()
  let key = ContextKey::new("test.key")
  let ctx_with_value = Context::with_value(ctx, key, "test_value")
  
  let retrieved_value = Context::get(ctx_with_value, key)
  assert_eq(retrieved_value, Some("test_value"))
}