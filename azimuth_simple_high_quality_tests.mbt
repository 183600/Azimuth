// Azimuth Telemetry System - Simple High Quality Test Suite
// This file contains 5 simple high-quality test cases covering basic telemetry functionality

// Test 1: Basic AttributeValue Type Operations
test "basic attribute value type operations" {
  // Test string attribute value
  let string_attr = StringValue("test_value")
  match string_attr {
    StringValue(v) => assert_eq(v, "test_value")
    _ => assert_true(false)
  }
  
  // Test int attribute value
  let int_attr = IntValue(42)
  match int_attr {
    IntValue(v) => assert_eq(v, 42)
    _ => assert_true(false)
  }
  
  // Test float attribute value
  let float_attr = FloatValue(3.14)
  match float_attr {
    FloatValue(v) => assert_eq(v, 3.14)
    _ => assert_true(false)
  }
  
  // Test bool attribute value
  let bool_attr = BoolValue(true)
  match bool_attr {
    BoolValue(v) => assert_true(v)
    _ => assert_true(false)
  }
  
  // Test array string attribute value
  let array_string_attr = ArrayStringValue(["a", "b", "c"])
  match array_string_attr {
    ArrayStringValue(v) => {
      assert_eq(v.length(), 3)
      assert_eq(v[0], "a")
      assert_eq(v[1], "b")
      assert_eq(v[2], "c")
    }
    _ => assert_true(false)
  }
  
  // Test array int attribute value
  let array_int_attr = ArrayIntValue([1, 2, 3])
  match array_int_attr {
    ArrayIntValue(v) => {
      assert_eq(v.length(), 3)
      assert_eq(v[0], 1)
      assert_eq(v[1], 2)
      assert_eq(v[2], 3)
    }
    _ => assert_true(false)
  }
}

// Test 2: Basic Attributes Operations
test "basic attributes operations" {
  let attrs = Attributes::new()
  
  // Test setting and getting string attribute
  Attributes::set(attrs, "string.key", StringValue("test_value"))
  let string_result = Attributes::get(attrs, "string.key")
  match string_result {
    Some(StringValue(v)) => assert_eq(v, "test_value")
    _ => assert_true(false)
  }
  
  // Test setting and getting int attribute
  Attributes::set(attrs, "int.key", IntValue(42))
  let int_result = Attributes::get(attrs, "int.key")
  match int_result {
    Some(IntValue(v)) => assert_eq(v, 42)
    _ => assert_true(false)
  }
  
  // Test getting non-existent attribute
  let non_existent = Attributes::get(attrs, "non.existent.key")
  match non_existent {
    Some(_) => assert_true(false)
    None => assert_true(true)
  }
}

// Test 3: Basic SpanContext Operations
test "basic span context operations" {
  let trace_id = "0af7651916cd43dd8448eb211c80319c"
  let span_id = "b7ad6b7169203331"
  let span_ctx = SpanContext::new(trace_id, span_id, true, "test_state")
  
  // Test trace_id retrieval
  assert_eq(SpanContext::trace_id(span_ctx), trace_id)
  
  // Test span_id retrieval
  assert_eq(SpanContext::span_id(span_ctx), span_id)
  
  // Test validity check
  assert_true(SpanContext::is_valid(span_ctx))
  
  // Test sampling check
  assert_true(SpanContext::is_sampled(span_ctx))
  
  // Test invalid span context
  let invalid_ctx = SpanContext::new("", "", false, "")
  assert_false(SpanContext::is_valid(invalid_ctx))
  assert_false(SpanContext::is_sampled(invalid_ctx))
}

// Test 4: Basic Span Operations
test "basic span operations" {
  let span_ctx = SpanContext::new("trace_id", "span_id", true, "")
  let span = Span::new("test_span", Internal, span_ctx)
  
  // Test span name
  assert_eq(Span::name(span), "test_span")
  
  // Test span kind
  match Span::kind(span) {
    Internal => assert_true(true)
    _ => assert_true(false)
  }
  
  // Test recording state
  assert_true(Span::is_recording(span))
  
  // Test span context
  let retrieved_ctx = Span::span_context(span)
  assert_eq(SpanContext::trace_id(retrieved_ctx), "trace_id")
  assert_eq(SpanContext::span_id(retrieved_ctx), "span_id")
  
  // Test span status
  assert_eq(Span::status(span), Unset)
  
  // Test span operations
  Span::set_status(span, Ok, Some("Operation completed"))
  Span::add_event(span, "test_event", Some([("test_attr", StringValue("test_value"))]))
  Span::end(span)
}

// Test 5: Basic Context Operations
test "basic context operations" {
  // Test root context
  let root_ctx = Context::root()
  match root_ctx.data {
    None => assert_true(true)
    Some(_) => assert_true(false)
  }
  
  // Test context with value
  let key = ContextKey::new("test_key")
  let ctx_with_value = Context::with_value(root_ctx, key, "test_value")
  match Context::get(ctx_with_value, key) {
    Some(value) => assert_eq(value, "test_value")
    None => assert_true(false)
  }
  
  // Test context with different key
  let different_key = ContextKey::new("different_key")
  match Context::get(ctx_with_value, different_key) {
    Some(_) => assert_true(false)
    None => assert_true(true)
  }
}