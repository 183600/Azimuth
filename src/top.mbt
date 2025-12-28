// Top-level module for Azimuth telemetry package
pub test "basic project setup test" {
  // Basic test to ensure project setup is working
  assert_eq(1 + 1, 2)
  assert_true(true)
  assert_false(false)
}

// Test 1: Attributes operations
pub test "attributes creation and operations" {
  let attrs = azimuth::Attributes::new()
  azimuth::Attributes::set(attrs, "string.key", azimuth::StringValue("test_value"))
  azimuth::Attributes::set(attrs, "int.key", azimuth::IntValue(42))
  
  let string_val = azimuth::Attributes::get(attrs, "string.key")
  let int_val = azimuth::Attributes::get(attrs, "int.key")
  let missing_val = azimuth::Attributes::get(attrs, "missing.key")
  
  assert_eq(string_val, Some(azimuth::StringValue("test_value")))
  assert_eq(int_val, Some(azimuth::IntValue(42)))
  assert_eq(missing_val, None)
}

// Test 2: Context and Baggage operations
pub test "context and baggage propagation" {
  let root_ctx = azimuth::Context::root()
  let key = azimuth::ContextKey::new("user.id")
  let ctx_with_value = azimuth::Context::with_value(root_ctx, key, "12345")
  
  let retrieved_value = azimuth::Context::get(ctx_with_value, key)
  assert_eq(retrieved_value, Some("12345"))
  
  let missing_key = azimuth::ContextKey::new("missing.key")
  let missing_value = azimuth::Context::get(root_ctx, missing_key)
  assert_eq(missing_value, None)
  
  // Test baggage operations
  let baggage = azimuth::Baggage::new()
  let updated_baggage = azimuth::Baggage::set_entry(baggage, "request.id", "req-123")
  let baggage_value = azimuth::Baggage::get_entry(updated_baggage, "request.id")
  assert_eq(baggage_value, Some("req-123"))
}

// Test 3: Span and Tracer functionality
pub test "span and tracer operations" {
  let span_ctx = azimuth::SpanContext::new("trace-123", "span-456", true, "")
  assert_eq(azimuth::SpanContext::trace_id(span_ctx), "trace-123")
  assert_eq(azimuth::SpanContext::span_id(span_ctx), "span-456")
  assert_true(azimuth::SpanContext::is_valid(span_ctx))
  assert_true(azimuth::SpanContext::is_sampled(span_ctx))
  
  let span = azimuth::Span::new("test-span", azimuth::Internal, span_ctx)
  assert_eq(azimuth::Span::name(span), "test-span")
  assert_eq(azimuth::Span::kind(span), azimuth::Internal)
  assert_true(azimuth::Span::is_recording(span))
  
  let provider = azimuth::TracerProvider::default()
  let tracer = azimuth::TracerProvider::get_tracer(provider, "test-tracer")
  let scope = azimuth::Tracer::instrumentation_scope(tracer)
  assert_eq(scope.name, "test-tracer")
  
  let new_span = azimuth::Tracer::start_span(tracer, "operation")
  assert_eq(azimuth::Span::name(new_span), "operation")
}