// Azimuth Telemetry System - Span State and Events Test
// Comprehensive test suite for Span lifecycle, state management, and events

test "span_creation_with_different_kinds" {
  let span_ctx = SpanContext::new("trace123", "span456", true, "")
  
  // Test Internal span
  let internal_span = Span::new("internal-operation", Internal, span_ctx)
  @assertion.assert_eq(Span::name(internal_span), "internal-operation")?
  @assertion.assert_eq(Span::kind(internal_span), Internal)?
  @assertion.assert_eq(Span::is_recording(internal_span), true)?
  
  // Test Server span
  let server_span = Span::new("server-operation", Server, span_ctx)
  @assertion.assert_eq(Span::name(server_span), "server-operation")?
  @assertion.assert_eq(Span::kind(server_span), Server)?
  @assertion.assert_eq(Span::is_recording(server_span), true)?
  
  // Test Client span
  let client_span = Span::new("client-operation", Client, span_ctx)
  @assertion.assert_eq(Span::name(client_span), "client-operation")?
  @assertion.assert_eq(Span::kind(client_span), Client)?
  @assertion.assert_eq(Span::is_recording(client_span), true)?
  
  // Test Producer span
  let producer_span = Span::new("producer-operation", Producer, span_ctx)
  @assertion.assert_eq(Span::name(producer_span), "producer-operation")?
  @assertion.assert_eq(Span::kind(producer_span), Producer)?
  @assertion.assert_eq(Span::is_recording(producer_span), true)?
  
  // Test Consumer span
  let consumer_span = Span::new("consumer-operation", Consumer, span_ctx)
  @assertion.assert_eq(Span::name(consumer_span), "consumer-operation")?
  @assertion.assert_eq(Span::kind(consumer_span), Consumer)?
  @assertion.assert_eq(Span::is_recording(consumer_span), true)?
}

test "span_status_management" {
  let span_ctx = SpanContext::new("trace123", "span456", true, "")
  let span = Span::new("test-span", Internal, span_ctx)
  
  // Test initial status
  @assertion.assert_eq(Span::status(span), Unset)?
  
  // Test setting Ok status
  Span::set_status(span, Ok, Some("Operation completed successfully"))
  @assertion.assert_eq(Span::status(span), Ok)?
  
  // Test setting Error status
  Span::set_status(span, Error, Some("Operation failed with error"))
  @assertion.assert_eq(Span::status(span), Error)?
  
  // Test setting Unset status
  Span::set_status(span, Unset, None)
  @assertion.assert_eq(Span::status(span), Unset)?
}

test "span_event_addition" {
  let span_ctx = SpanContext::new("trace123", "span456", true, "")
  let span = Span::new("test-span", Internal, span_ctx)
  
  // Test adding simple event
  Span::add_event(span, "event1", None)
  
  // Test adding event with attributes
  let event_attributes = [
    ("event.type", StringValue("user.action")),
    ("user.id", IntValue(12345)),
    ("action.success", BoolValue(true))
  ]
  Span::add_event(span, "user.login", Some(event_attributes))
  
  // Test adding multiple events
  Span::add_event(span, "database.query", Some([("query.type", StringValue("SELECT"))]))
  Span::add_event(span, "cache.miss", Some([("cache.key", StringValue("user:12345"))]))
  
  // Verify span is still recording after events
  @assertion.assert_eq(Span::is_recording(span), true)?
}

test "span_lifecycle_transitions" {
  let span_ctx = SpanContext::new("trace123", "span456", true, "")
  let span = Span::new("lifecycle-test", Internal, span_ctx)
  
  // Test initial state
  @assertion.assert_eq(Span::is_recording(span), true)?
  @assertion.assert_eq(Span::name(span), "lifecycle-test")?
  
  // Test operations during recording
  Span::set_status(span, Ok, Some("In progress"))
  Span::add_event(span, "milestone1", None)
  @assertion.assert_eq(Span::is_recording(span), true)?
  
  // Test span ending
  Span::end(span)
  // Note: In a real implementation, is_recording might return false after end
  // For this simplified test, we just verify the operation doesn't crash
}

test "span_context_operations" {
  let span_ctx = SpanContext::new("trace123", "span456", true, "state=value")
  let span = Span::new("context-test", Internal, span_ctx)
  
  // Test accessing span context
  let retrieved_ctx = Span::span_context(span)
  @assertion.assert_eq(SpanContext::trace_id(retrieved_ctx), "trace123")?
  @assertion.assert_eq(SpanContext::span_id(retrieved_ctx), "span456")?
  @assertion.assert_eq(SpanContext::is_sampled(retrieved_ctx), true)?
  
  // Test context validity
  @assertion.assert_eq(SpanContext::is_valid(retrieved_ctx), true)?
  
  // Test context with empty IDs
  let empty_ctx = SpanContext::new("", "", false, "")
  let empty_span = Span::new("empty-context", Internal, empty_ctx)
  let retrieved_empty_ctx = Span::span_context(empty_span)
  @assertion.assert_eq(SpanContext::is_valid(retrieved_empty_ctx), false)?
}

test "span_with_complex_attributes" {
  let provider = TracerProvider::default()
  let tracer = TracerProvider::get_tracer(provider, "test-tracer")
  let span = Tracer::start_span(tracer, "complex-attributes")
  
  // Test adding events with complex attributes
  let complex_attributes = [
    ("string.attr", StringValue("complex value")),
    ("int.attr", IntValue(42)),
    ("float.attr", FloatValue(3.14159)),
    ("bool.attr", BoolValue(true)),
    ("array.string", ArrayStringValue(["item1", "item2", "item3"])),
    ("array.int", ArrayIntValue([1, 2, 3, 4, 5]))
  ]
  
  Span::add_event(span, "complex.event", Some(complex_attributes))
  Span::set_status(span, Ok, Some("Complex operation completed"))
  
  @assertion.assert_eq(Span::is_recording(span), true)?
}

test "span_error_handling_scenarios" {
  let span_ctx = SpanContext::new("trace123", "span456", true, "")
  let span = Span::new("error-test", Internal, span_ctx)
  
  // Test error status with description
  Span::set_status(span, Error, Some("Database connection failed"))
  @assertion.assert_eq(Span::status(span), Error)?
  
  // Test error events
  let error_attributes = [
    ("error.type", StringValue("DatabaseError")),
    ("error.code", IntValue(500)),
    ("retry.count", IntValue(3)),
    ("error.recoverable", BoolValue(true))
  ]
  Span::add_event(span, "error.occurred", Some(error_attributes))
  
  // Test recovery event
  let recovery_attributes = [
    ("recovery.action", StringValue("reconnect")),
    ("recovery.success", BoolValue(true))
  ]
  Span::add_event(span, "error.recovered", Some(recovery_attributes))
  
  // Final status
  Span::set_status(span, Ok, Some("Error recovered successfully"))
  @assertion.assert_eq(Span::status(span), Ok)?
}

test "span_timing_and_duration" {
  let span_ctx = SpanContext::new("trace123", "span456", true, "")
  let span = Span::new("timing-test", Internal, span_ctx)
  
  // Test timing events
  Span::add_event(span, "operation.started", Some([("timestamp", StringValue("2025-01-01T00:00:00Z"))]))
  
  // Simulate some work by adding events
  for i in range(0, 5) {
    let event_name = "work.step." + i.to_string()
    let attributes = [("step.number", IntValue(i)), ("step.completed", BoolValue(true))]
    Span::add_event(span, event_name, Some(attributes))
  }
  
  Span::add_event(span, "operation.completed", Some([("duration.ms", IntValue(100))]))
  
  // End the span
  Span::end(span)
}

test "span_nested_operations" {
  let provider = TracerProvider::default()
  let tracer = TracerProvider::get_tracer(provider, "nested-test")
  
  // Create parent span
  let parent_span = Tracer::start_span(tracer, "parent-operation")
  Span::add_event(parent_span, "parent.started", None)
  
  // Simulate nested operations (in real implementation, these would have parent-child relationships)
  let child_span1 = Tracer::start_span(tracer, "child-operation-1")
  Span::add_event(child_span1, "child1.started", None)
  Span::set_status(child_span1, Ok, Some("Child 1 completed"))
  Span::end(child_span1)
  
  let child_span2 = Tracer::start_span(tracer, "child-operation-2")
  Span::add_event(child_span2, "child2.started", None)
  Span::set_status(child_span2, Ok, Some("Child 2 completed"))
  Span::end(child_span2)
  
  // Complete parent span
  Span::add_event(parent_span, "parent.children_completed", None)
  Span::set_status(parent_span, Ok, Some("All operations completed"))
  Span::end(parent_span)
}

test "span_concurrent_operations" {
  let provider = TracerProvider::default()
  let tracer = TracerProvider::get_tracer(provider, "concurrent-test")
  
  // Create multiple spans to simulate concurrent operations
  let spans = [for i in range(0, 3) {
    let span = Tracer::start_span(tracer, "concurrent-operation-" + i.to_string())
    Span::add_event(span, "started", Some([("operation.id", IntValue(i))]))
    span
  }]
  
  // Process each span
  for i in range(0, 3) {
    let span = spans[i]
    Span::add_event(span, "processing", Some([("operation.id", IntValue(i))]))
    Span::set_status(span, Ok, Some("Operation " + i.to_string() + " completed"))
    Span::end(span)
  }
}

test "span_with_different_sampling_decisions" {
  // Test span with sampling enabled
  let sampled_ctx = SpanContext::new("trace123", "span456", true, "")
  let sampled_span = Span::new("sampled-span", Internal, sampled_ctx)
  @assertion.assert_eq(SpanContext::is_sampled(Span::span_context(sampled_span)), true)?
  
  // Test span with sampling disabled
  let not_sampled_ctx = SpanContext::new("trace789", "span012", false, "")
  let not_sampled_span = Span::new("not-sampled-span", Internal, not_sampled_ctx)
  @assertion.assert_eq(SpanContext::is_sampled(Span::span_context(not_sampled_span)), false)?
  
  // Both spans should still be recordable
  @assertion.assert_eq(Span::is_recording(sampled_span), true)?
  @assertion.assert_eq(Span::is_recording(not_sampled_span), true)?
}