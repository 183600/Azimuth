// Span Context Validation Tests
// Test cases for comprehensive span context creation, validation, and edge cases

test "span_context_creation_with_valid_inputs" {
  // Test creating span context with valid trace and span IDs
  let trace_id = "0af7651916cd43dd8448eb211c80319c"
  let span_id = "b7ad6b7169203331"
  let span_ctx = SpanContext::new(trace_id, span_id, true, "key1=value1,key2=value2")
  
  // Verify all fields are correctly set
  @assertion.assert_eq(span_ctx.trace_id, trace_id)?
  @assertion.assert_eq(span_ctx.span_id, span_id)?
  @assertion.assert_eq(span_ctx.sampled, true)?
  @assertion.assert_eq(span_ctx.trace_state, "key1=value1,key2=value2")?
}

test "span_context_validation_functions" {
  // Test span context validation functions
  let valid_ctx = SpanContext::new("0af7651916cd43dd8448eb211c80319c", "b7ad6b7169203331", true, "")
  let invalid_empty_trace_ctx = SpanContext::new("", "b7ad6b7169203331", true, "")
  let invalid_empty_span_ctx = SpanContext::new("0af7651916cd43dd8448eb211c80319c", "", true, "")
  
  // Test is_valid function
  @assertion.assert_eq(SpanContext::is_valid(valid_ctx), true)?
  @assertion.assert_eq(SpanContext::is_valid(invalid_empty_trace_ctx), false)?
  @assertion.assert_eq(SpanContext::is_valid(invalid_empty_span_ctx), false)?
  
  // Test is_sampled function
  @assertion.assert_eq(SpanContext::is_sampled(valid_ctx), true)?
  
  let not_sampled_ctx = SpanContext::new("0af7651916cd43dd8448eb211c80319c", "b7ad6b7169203331", false, "")
  @assertion.assert_eq(SpanContext::is_sampled(not_sampled_ctx), false)?
}

test "span_context_accessor_methods" {
  // Test span context accessor methods
  let trace_id = "1234567890abcdef1234567890abcdef"
  let span_id = "fedcba0987654321"
  let span_ctx = SpanContext::new(trace_id, span_id, true, "test=value")
  
  // Test accessor methods
  @assertion.assert_eq(SpanContext::trace_id(span_ctx), trace_id)?
  @assertion.assert_eq(SpanContext::span_id(span_ctx), span_id)?
  @assertion.assert_eq(SpanContext::is_sampled(span_ctx), true)?
}

test "span_context_edge_cases" {
  // Test edge cases for span context creation
  let minimal_valid_ctx = SpanContext::new("1", "1", false, "")
  @assertion.assert_eq(SpanContext::is_valid(minimal_valid_ctx), true)?
  @assertion.assert_eq(SpanContext::is_sampled(minimal_valid_ctx), false)?
  
  // Test with complex trace state
  let complex_trace_state = "key1=value1,key2=value2;key3=value3"
  let complex_ctx = SpanContext::new("0af7651916cd43dd8448eb211c80319c", "b7ad6b7169203331", true, complex_trace_state)
  @assertion.assert_eq(complex_ctx.trace_state, complex_trace_state)?
}

test "span_context_id_format_consistency" {
  // Test that span context maintains ID format consistency
  let hex_trace_id = "0af7651916cd43dd8448eb211c80319c"
  let hex_span_id = "b7ad6b7169203331"
  let span_ctx = SpanContext::new(hex_trace_id, hex_span_id, true, "")
  
  // Verify IDs are preserved exactly as provided
  @assertion.assert_eq(SpanContext::trace_id(span_ctx), hex_trace_id)?
  @assertion.assert_eq(SpanContext::span_id(span_ctx), hex_span_id)?
  
  // Test with different sampling states
  let sampled_ctx = SpanContext::new(hex_trace_id, hex_span_id, true, "")
  let not_sampled_ctx = SpanContext::new(hex_trace_id, hex_span_id, false, "")
  
  @assertion.assert_eq(SpanContext::is_sampled(sampled_ctx), true)?
  @assertion.assert_eq(SpanContext::is_sampled(not_sampled_ctx), false)?
  @assertion.assert_eq(SpanContext::trace_id(sampled_ctx), hex_trace_id)?
  @assertion.assert_eq(SpanContext::trace_id(not_sampled_ctx), hex_trace_id)?
}