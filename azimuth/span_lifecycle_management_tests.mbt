// Span State Management Test Suite for Azimuth Telemetry System
// Tests span lifecycle, status management, and event handling

test "span lifecycle management" {
  let span_ctx = SpanContext::new("trace-123", "span-456", true, "")
  let span = Span::new("test-operation", Internal, span_ctx)
  
  // Verify initial state
  assert_eq(Span::name(span), "test-operation")
  assert_eq(Span::kind(span), Internal)
  assert_true(Span::is_recording(span))
  
  // Test setting status
  Span::set_status(span, Ok, Some("Operation completed successfully"))
  assert_eq(Span::status(span), Ok)
  
  // Test adding events
  Span::add_event(span, "event1", Some([("key1", StringValue("value1"))]))
  Span::add_event(span, "event2", Some([("key2", IntValue(42))]))
  
  // Test ending span
  Span::end(span)
  // After ending, the span should still maintain its data
  assert_eq(Span::name(span), "test-operation")
}

test "span with different kinds" {
  let span_ctx = SpanContext::new("trace-123", "span-456", true, "")
  
  let server_span = Span::new("server-operation", Server, span_ctx)
  assert_eq(Span::kind(server_span), Server)
  
  let client_span = Span::new("client-operation", Client, span_ctx)
  assert_eq(Span::kind(client_span), Client)
  
  let producer_span = Span::new("producer-operation", Producer, span_ctx)
  assert_eq(Span::kind(producer_span), Producer)
  
  let consumer_span = Span::new("consumer-operation", Consumer, span_ctx)
  assert_eq(Span::kind(consumer_span), Consumer)
  
  let internal_span = Span::new("internal-operation", Internal, span_ctx)
  assert_eq(Span::kind(internal_span), Internal)
}

test "span status transitions" {
  let span_ctx = SpanContext::new("trace-123", "span-456", true, "")
  let span = Span::new("status-test", Internal, span_ctx)
  
  // Initial status should be Unset
  assert_eq(Span::status(span), Unset)
  
  // Transition to Ok
  Span::set_status(span, Ok)
  assert_eq(Span::status(span), Ok)
  
  // Transition to Error
  Span::set_status(span, Error, Some("Something went wrong"))
  assert_eq(Span::status(span), Error)
  
  // Transition back to Ok
  Span::set_status(span, Ok, Some("Recovered"))
  assert_eq(Span::status(span), Ok)
}

test "span event handling with complex attributes" {
  let span_ctx = SpanContext::new("trace-123", "span-456", true, "")
  let span = Span::new("event-test", Internal, span_ctx)
  
  // Add event with multiple attributes
  let complex_attributes = [
    ("user.id", StringValue("user-123")),
    ("request.count", IntValue(5)),
    ("processing.time", FloatValue(125.5)),
    ("cache.hit", BoolValue(true)),
    ("tags", ArrayStringValue(["tag1", "tag2", "tag3"]))
  ]
  
  Span::add_event(span, "complex.event", Some(complex_attributes))
  
  // Add event without attributes
  Span::add_event(span, "simple.event", None)
  
  // Add event with empty attributes
  Span::add_event(span, "empty.event", Some([]))
  
  // Verify span is still recording
  assert_true(Span::is_recording(span))
}