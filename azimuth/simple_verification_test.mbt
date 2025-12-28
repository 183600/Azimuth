// Simple test to verify basic functionality works
test "basic test verification" {
  assert_eq(1 + 1, 2)
  assert_true(true)
  assert_false(false)
}

test "span context basic test" {
  let span_ctx = SpanContext::new("trace_id", "span_id", true, "")
  assert_true(SpanContext::is_valid(span_ctx))
  assert_eq(SpanContext::trace_id(span_ctx), "trace_id")
  assert_eq(SpanContext::span_id(span_ctx), "span_id")
}