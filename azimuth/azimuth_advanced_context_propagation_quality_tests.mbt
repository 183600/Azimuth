// Azimuth Advanced Context Propagation Quality Tests
// Tests for complex cross-service context propagation scenarios

test "context propagation with multiple service hops" {
  // Arrange - Create initial context with trace and baggage
  let provider = TracerProvider::default()
  let tracer = TracerProvider::get_tracer(provider, "service-a")
  let span = Tracer::start_span(tracer, "operation-a")
  let context = Span::context(span)
  
  // Add baggage items
  let baggage = Context::baggage(context)
  let updated_baggage = Baggage::set(baggage, "user-id", "12345")
  let updated_baggage = Baggage::set(updated_baggage, "request-id", "req-abc-789")
  let context_with_baggage = Context::with_baggage(context, updated_baggage)
  
  // Act - Simulate service B receiving and forwarding context
  let propagator = W3CTraceContextPropagator::new()
  let carrier = Array[(String, String)]::empty()
  
  // Inject context into carrier
  TextMapPropagator::inject(propagator, context_with_baggage, carrier)
  
  // Extract context in service B
  let extracted_context = TextMapPropagator::extract(propagator, carrier)
  
  // Add new baggage in service B
  let extracted_baggage = Context::baggage(extracted_context)
  let service_b_baggage = Baggage::set(extracted_baggage, "service-b-timestamp", "1640995200")
  let context_b = Context::with_baggage(extracted_context, service_b_baggage)
  
  // Forward to service C
  let carrier_c = Array[(String, String)]::empty()
  TextMapPropagator::inject(propagator, context_b, carrier_c)
  
  // Extract in service C
  let context_c = TextMapPropagator::extract(propagator, carrier_c)
  
  // Assert - Verify all baggage items propagated correctly
  let final_baggage = Context::baggage(context_c)
  assert_eq(Baggage::get(final_baggage, "user-id"), Some("12345"))
  assert_eq(Baggage::get(final_baggage, "request-id"), Some("req-abc-789"))
  assert_eq(Baggage::get(final_baggage, "service-b-timestamp"), Some("1640995200"))
  
  // Verify trace context is maintained across all hops
  let span_context = Context::span_context(context_c)
  assert_true(SpanContext::is_valid(span_context))
  assert_eq(SpanContext::trace_id(span_context), SpanContext::trace_id(Span::context(span)))
}

test "context propagation with special characters and encoding" {
  // Arrange - Create context with special characters in baggage
  let provider = TracerProvider::default()
  let tracer = TracerProvider::get_tracer(provider, "encoding-test")
  let span = Tracer::start_span(tracer, "operation-with-special-chars")
  let context = Span::context(span)
  
  // Add baggage with special characters, spaces, and unicode
  let baggage = Context::baggage(context)
  let baggage = Baggage::set(baggage, "user-name", "John Doe")
  let baggage = Baggage::set(baggage, "query", "SELECT * FROM users WHERE active = true")
  let baggage = Baggage::set(baggage, "unicode", "测试中文")
  let baggage = Baggage::set(baggage, "emoji", "🚀🌟")
  let baggage = Baggage::set(baggage, "url-encoded", "https://example.com/path?param=value&other=123")
  let context_with_baggage = Context::with_baggage(context, baggage)
  
  // Act - Propagate through HTTP headers
  let propagator = W3CTraceContextPropagator::new()
  let baggage_propagator = W3CBaggagePropagator::new()
  let composite = CompositePropagator::new([propagator, baggage_propagator])
  
  let carrier = Array[(String, String)]::empty()
  TextMapPropagator::inject(composite, context_with_baggage, carrier)
  
  // Extract and verify
  let extracted_context = TextMapPropagator::extract(composite, carrier)
  let extracted_baggage = Context::baggage(extracted_context)
  
  // Assert - Verify special characters are preserved
  assert_eq(Baggage::get(extracted_baggage, "user-name"), Some("John Doe"))
  assert_eq(Baggage::get(extracted_baggage, "query"), Some("SELECT * FROM users WHERE active = true"))
  assert_eq(Baggage::get(extracted_baggage, "unicode"), Some("测试中文"))
  assert_eq(Baggage::get(extracted_baggage, "emoji"), Some("🚀🌟"))
  assert_eq(Baggage::get(extracted_baggage, "url-encoded"), Some("https://example.com/path?param=value&other=123"))
}

test "context propagation with baggage size limits" {
  // Arrange - Create context with large baggage
  let provider = TracerProvider::default()
  let tracer = TracerProvider::get_tracer(provider, "size-limit-test")
  let span = Tracer::start_span(tracer, "large-baggage-operation")
  let context = Span::context(span)
  
  // Add many baggage items to test size limits
  let baggage = Context::baggage(context)
  let baggage = Baggage::set(baggage, "large-value-1", "A".repeat(1000))
  let baggage = Baggage::set(baggage, "large-value-2", "B".repeat(2000))
  let baggage = Baggage::set(baggage, "large-value-3", "C".repeat(3000))
  let context_with_baggage = Context::with_baggage(context, baggage)
  
  // Act - Propagate through multiple carriers
  let propagator = W3CBaggagePropagator::new()
  let carrier = Array[(String, String)]::empty()
  TextMapPropagator::inject(propagator, context_with_baggage, carrier)
  
  // Extract
  let extracted_context = TextMapPropagator::extract(propagator, carrier)
  let extracted_baggage = Context::baggage(extracted_context)
  
  // Assert - Verify large baggage items are handled correctly
  assert_eq(Baggage::get(extracted_baggage, "large-value-1"), Some("A".repeat(1000)))
  assert_eq(Baggage::get(extracted_baggage, "large-value-2"), Some("B".repeat(2000)))
  assert_eq(Baggage::get(extracted_baggage, "large-value-3"), Some("C".repeat(3000)))
}

test "context propagation with carrier corruption handling" {
  // Arrange - Create valid context
  let provider = TracerProvider::default()
  let tracer = TracerProvider::get_tracer(provider, "corruption-test")
  let span = Tracer::start_span(tracer, "corruption-operation")
  let context = Span::context(span)
  
  // Act - Simulate corrupted carrier headers
  let propagator = W3CTraceContextPropagator::new()
  let corrupted_carrier = Array[(String, String)]::from([
    ("traceparent", "invalid-format-corrupted"),
    ("tracestate", "invalid=tracestate="),
    ("baggage", "invalid=baggage=unclosed-quote")
  ])
  
  // Extract should handle corruption gracefully
  let extracted_context = TextMapPropagator::extract(propagator, corrupted_carrier)
  
  // Assert - Should fallback to empty context instead of crashing
  let span_context = Context::span_context(extracted_context)
  assert_false(SpanContext::is_valid(span_context))
}

test "context propagation with custom carrier types" {
  // Arrange - Create context with baggage
  let provider = TracerProvider::default()
  let tracer = TracerProvider::get_tracer(provider, "custom-carrier-test")
  let span = Tracer::start_span(tracer, "custom-carrier-operation")
  let baggage = Context::baggage(Span::context(span))
  let baggage = Baggage::set(baggage, "custom-key", "custom-value")
  let context = Context::with_baggage(Span::context(span), baggage)
  
  // Act - Use custom carrier implementation
  let propagator = W3CBaggagePropagator::new()
  let custom_carrier = Array[(String, String)]::from([
    ("custom-header-1", "custom-value-1"),
    ("custom-header-2", "custom-value-2")
  ])
  
  TextMapPropagator::inject(propagator, context, custom_carrier)
  
  // Extract from custom carrier
  let extracted_context = TextMapPropagator::extract(propagator, custom_carrier)
  let extracted_baggage = Context::baggage(extracted_context)
  
  // Assert - Verify custom carrier works correctly
  assert_eq(Baggage::get(extracted_baggage, "custom-key"), Some("custom-value"))
}

test "context propagation with nested operations" {
  // Arrange - Create parent span
  let provider = TracerProvider::default()
  let tracer = TracerProvider::get_tracer(provider, "nested-test")
  let parent_span = Tracer::start_span(tracer, "parent-operation")
  let parent_context = Span::context(parent_span)
  
  // Act - Create nested child spans with different baggage
  let child1_span = Tracer::start_span_with_context(tracer, "child-1", parent_context)
  let child1_context = Span::context(child1_span)
  let child1_baggage = Baggage::set(Context::baggage(child1_context), "child1-data", "value1")
  let child1_final = Context::with_baggage(child1_context, child1_baggage)
  
  let child2_span = Tracer::start_span_with_context(tracer, "child-2", child1_final)
  let child2_context = Span::context(child2_span)
  let child2_baggage = Baggage::set(Context::baggage(child2_context), "child2-data", "value2")
  let child2_final = Context::with_baggage(child2_context, child2_baggage)
  
  // Propagate nested context
  let propagator = CompositePropagator::new([
    W3CTraceContextPropagator::new(),
    W3CBaggagePropagator::new()
  ])
  
  let carrier = Array[(String, String)]::empty()
  TextMapPropagator::inject(propagator, child2_final, carrier)
  
  // Extract and verify hierarchy
  let extracted_context = TextMapPropagator::extract(propagator, carrier)
  let extracted_baggage = Context::baggage(extracted_context)
  let extracted_span_context = Context::span_context(extracted_context)
  
  // Assert - Verify nested context is preserved
  assert_eq(Baggage::get(extracted_baggage, "child1-data"), Some("value1"))
  assert_eq(Baggage::get(extracted_baggage, "child2-data"), Some("value2"))
  assert_true(SpanContext::is_valid(extracted_span_context))
  assert_eq(SpanContext::trace_id(extracted_span_context), SpanContext::trace_id(Span::context(parent_span)))
}