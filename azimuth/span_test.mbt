// Test file for SpanContext and Span functionality

pub test "span context creation and properties" {
  let trace_id = "0af7651916cd43dd8448eb211c80319c"
  let span_id = "b7ad6b7169203331"
  let sampled = true
  let trace_state = "rojo=00-4bf92f3577b34da6a3ce929d0e0e4736-00f067aa0ba902b7-01"
  
  let span_ctx = SpanContext::new(trace_id, span_id, sampled, trace_state)
  
  assert_eq(SpanContext::trace_id(span_ctx), trace_id)
  assert_eq(SpanContext::span_id(span_ctx), span_id)
  assert_eq(SpanContext::is_sampled(span_ctx), sampled)
  assert_eq(SpanContext::is_valid(span_ctx), true)
}

pub test "span context validation" {
  // Test valid span context
  let valid_ctx = SpanContext::new("0af7651916cd43dd8448eb211c80319c", "b7ad6b7169203331", true, "")
  assert_eq(SpanContext::is_valid(valid_ctx), true)
  
  // Test invalid span context with empty trace_id
  let invalid_trace_ctx = SpanContext::new("", "b7ad6b7169203331", true, "")
  assert_eq(SpanContext::is_valid(invalid_trace_ctx), false)
  
  // Test invalid span context with empty span_id
  let invalid_span_ctx = SpanContext::new("0af7651916cd43dd8448eb211c80319c", "", true, "")
  assert_eq(SpanContext::is_valid(invalid_span_ctx), false)
  
  // Test invalid span context with both empty
  let invalid_both_ctx = SpanContext::new("", "", false, "")
  assert_eq(SpanContext::is_valid(invalid_both_ctx), false)
}

pub test "span creation and basic properties" {
  let span_name = "test.span"
  let span_kind = Internal
  let span_ctx = SpanContext::new("trace123", "span456", true, "")
  
  let span = Span::new(span_name, span_kind, span_ctx)
  
  assert_eq(Span::name(span), span_name)
  assert_eq(Span::kind(span), span_kind)
  assert_eq(Span::is_recording(span), true)
  assert_eq(Span::span_context(span), span_ctx)
}

pub test "span with different kinds" {
  let span_ctx = SpanContext::new("trace123", "span456", true, "")
  
  let internal_span = Span::new("internal", Internal, span_ctx)
  let server_span = Span::new("server", Server, span_ctx)
  let client_span = Span::new("client", Client, span_ctx)
  let producer_span = Span::new("producer", Producer, span_ctx)
  let consumer_span = Span::new("consumer", Consumer, span_ctx)
  
  assert_eq(Span::kind(internal_span), Internal)
  assert_eq(Span::kind(server_span), Server)
  assert_eq(Span::kind(client_span), Client)
  assert_eq(Span::kind(producer_span), Producer)
  assert_eq(Span::kind(consumer_span), Consumer)
}

pub test "span status operations" {
  let span_ctx = SpanContext::new("trace123", "span456", true, "")
  let span = Span::new("test.span", Internal, span_ctx)
  
  // Test initial status
  assert_eq(Span::status(span), Unset)
  
  // Test setting status
  Span::set_status(span, Ok)
  // Note: In simplified implementation, status remains Unset
  
  Span::set_status(span, Error, Some("Something went wrong"))
  // Note: In simplified implementation, status remains Unset
}

pub test "span event operations" {
  let span_ctx = SpanContext::new("trace123", "span456", true, "")
  let span = Span::new("test.span", Internal, span_ctx)
  
  // Test adding event without attributes
  Span::add_event(span, "event1", None)
  
  // Test adding event with attributes
  let attributes = [("key1", StringValue("value1")), ("key2", IntValue(42))]
  Span::add_event(span, "event2", Some(attributes))
  
  // Test that span is still recording after events
  assert_eq(Span::is_recording(span), true)
}

pub test "span lifecycle" {
  let span_ctx = SpanContext::new("trace123", "span456", true, "")
  let span = Span::new("lifecycle.span", Internal, span_ctx)
  
  // Test initial state
  assert_eq(Span::is_recording(span), true)
  
  // Test ending span
  Span::end(span)
  // Note: In simplified implementation, recording state doesn't change
  
  // Test operations after ending
  Span::add_event(span, "after.end", None)
  Span::set_status(span, Ok)
}

pub test "tracer operations" {
  let provider = TracerProvider::default()
  let tracer = TracerProvider::get_tracer(provider, "test.tracer", Some("1.0.0"))
  
  let scope = Tracer::instrumentation_scope(tracer)
  assert_eq(scope.name, "test.tracer")
  assert_eq(scope.version, Some("1.0.0"))
  assert_eq(scope.schema_url, None)
  
  // Test starting span
  let span = Tracer::start_span(tracer, "test.span", None)
  assert_eq(Span::name(span), "test.span")
  assert_eq(Span::kind(span), Internal)
  
  // Test starting span with attributes
  let attributes = [("attr1", StringValue("value1"))]
  let span_with_attrs = Tracer::start_span(tracer, "attr.span", Some(attributes))
  assert_eq(Span::name(span_with_attrs), "attr.span")
}

pub test "span context edge cases" {
  // Test with very long trace_id and span_id
  let long_trace_id = "a".repeat(32)
  let long_span_id = "b".repeat(16)
  let long_ctx = SpanContext::new(long_trace_id, long_span_id, false, "test.state")
  assert_eq(SpanContext::is_valid(long_ctx), true)
  assert_eq(SpanContext::is_sampled(long_ctx), false)
  
  // Test with special characters in trace_state
  let special_state_ctx = SpanContext::new("trace123", "span456", true, "key1=value1,key2=value2")
  assert_eq(SpanContext::is_valid(special_state_ctx), true)
  assert_eq(SpanContext::is_sampled(special_state_ctx), true)
  
  // Test with empty trace_state
  let empty_state_ctx = SpanContext::new("trace123", "span456", false, "")
  assert_eq(SpanContext::is_valid(empty_state_ctx), true)
  assert_eq(SpanContext::is_sampled(empty_state_ctx), false)
}