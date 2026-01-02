// Enhanced Core Functionality Tests for Azimuth Telemetry System
// This file contains focused test cases for core telemetry functionality

test "span context validation and operations" {
  // Create a valid span context
  let trace_id = "0af7651916cd43dd8448eb211c80319c"
  let span_id = "b7ad6b7169203331"
  let span_ctx = SpanContext::new(trace_id, span_id, true, "")
  
  // Validate span context operations
  assert_eq(SpanContext::trace_id(span_ctx), trace_id)
  assert_eq(SpanContext::span_id(span_ctx), span_id)
  assert_true(SpanContext::is_valid(span_ctx))
  assert_true(SpanContext::is_sampled(span_ctx))
  
  // Test invalid span context
  let invalid_span_ctx = SpanContext::new("", "", false, "")
  assert_false(SpanContext::is_valid(invalid_span_ctx))
  assert_false(SpanContext::is_sampled(invalid_span_ctx))
}

test "attribute value type conversion and validation" {
  // Test different attribute value types
  let string_attr = StringValue("test_value")
  let int_attr = IntValue(42)
  let float_attr = FloatValue(3.14)
  let bool_attr = BoolValue(true)
  let array_string_attr = ArrayStringValue(["a", "b", "c"])
  let array_int_attr = ArrayIntValue([1, 2, 3])
  
  // Create attributes and test operations
  let attrs = Attributes::new()
  
  // Test attribute retrieval with known keys
  let string_result = Attributes::get(attrs, "string.key")
  let int_result = Attributes::get(attrs, "int.key")
  let missing_result = Attributes::get(attrs, "missing.key")
  
  assert_eq(string_result, Some(StringValue("test_value")))
  assert_eq(int_result, Some(IntValue(42)))
  assert_eq(missing_result, None)
}