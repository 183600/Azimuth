// Enhanced Span Lifecycle and State Management Tests for Azimuth
// Tests span creation, state transitions, and lifecycle management

test "span creation with different kinds" {
  let span_ctx = SpanContext::new("trace-123", "span-456", true, "")
  
  // Test creating spans with different kinds
  let internal_span = Span::new("internal-operation", Internal, span_ctx)
  let server_span = Span::new("server-operation", Server, span_ctx)
  let client_span = Span::new("client-operation", Client, span_ctx)
  let producer_span = Span::new("producer-operation", Producer, span_ctx)
  let consumer_span = Span::new("consumer-operation", Consumer, span_ctx)
  
  // Verify span kinds
  assert_eq(Span::kind(internal_span), Internal)
  assert_eq(Span::kind(server_span), Server)
  assert_eq(Span::kind(client_span), Client)
  assert_eq(Span::kind(producer_span), Producer)
  assert_eq(Span::kind(consumer_span), Consumer)
  
  // Verify all spans are recording initially
  assert_true(Span::is_recording(internal_span))
  assert_true(Span::is_recording(server_span))
  assert_true(Span::is_recording(client_span))
  assert_true(Span::is_recording(producer_span))
  assert_true(Span::is_recording(consumer_span))
  
  // Verify span names
  assert_eq(Span::name(internal_span), "internal-operation")
  assert_eq(Span::name(server_span), "server-operation")
  assert_eq(Span::name(client_span), "client-operation")
  assert_eq(Span::name(producer_span), "producer-operation")
  assert_eq(Span::name(consumer_span), "consumer-operation")
}

test "span status code management" {
  let span_ctx = SpanContext::new("trace-123", "span-456", true, "")
  let span = Span::new("test-span", Internal, span_ctx)
  
  // Test initial status
  let initial_status = Span::status(span)
  assert_eq(initial_status, Unset)
  
  // Test setting status to Ok
  Span::set_status(span, Ok, Some("Operation completed successfully"))
  let ok_status = Span::status(span)
  assert_eq(ok_status, Ok)
  
  // Test setting status to Error
  Span::set_status(span, Error, Some("Operation failed with timeout"))
  let error_status = Span::status(span)
  assert_eq(error_status, Error)
  
  // Test setting status back to Unset
  Span::set_status(span, Unset, None)
  let unset_status = Span::status(span)
  assert_eq(unset_status, Unset)
}

test "span event management" {
  let span_ctx = SpanContext::new("trace-123", "span-456", true, "")
  let span = Span::new("test-span", Internal, span_ctx)
  
  // Test adding events without attributes
  Span::add_event(span, "event1", None)
  Span::add_event(span, "event2", None)
  
  // Test adding events with attributes
  let event1_attrs = [("key1", StringValue("value1")), ("key2", IntValue(42))]
  let event2_attrs = [("error", StringValue("timeout")), ("retry.count", IntValue(3))]
  
  Span::add_event(span, "error.event", Some(event1_attrs))
  Span::add_event(span, "retry.event", Some(event2_attrs))
  
  // Verify span is still recording after events
  assert_true(Span::is_recording(span))
}

test "span lifecycle end to end" {
  let span_ctx = SpanContext::new("trace-123", "span-456", true, "")
  let span = Span::new("lifecycle-test", Internal, span_ctx)
  
  // Verify initial state
  assert_true(Span::is_recording(span))
  assert_eq(Span::name(span), "lifecycle-test")
  assert_eq(Span::kind(span), Internal)
  
  // Add some events and status changes during span lifecycle
  Span::add_event(span, "start", None)
  Span::set_status(span, Ok, Some("In progress"))
  
  let progress_attrs = [("progress", FloatValue(0.5))]
  Span::add_event(span, "progress.update", Some(progress_attrs))
  
  // End the span
  Span::end(span)
  
  // After ending, verify span properties are still accessible
  assert_eq(Span::name(span), "lifecycle-test")
  assert_eq(Span::kind(span), Internal)
  assert_eq(Span::span_context(span), span_ctx)
}

test "span context validation and properties" {
  // Test valid span context
  let valid_trace_id = "0af7651916cd43dd8448eb211c80319c"
  let valid_span_id = "b7ad6b7169203331"
  let valid_ctx = SpanContext::new(valid_trace_id, valid_span_id, true, "key1=value1,key2=value2")
  
  assert_true(SpanContext::is_valid(valid_ctx))
  assert_true(SpanContext::is_sampled(valid_ctx))
  assert_eq(SpanContext::trace_id(valid_ctx), valid_trace_id)
  assert_eq(SpanContext::span_id(valid_ctx), valid_span_id)
  
  // Test invalid span context with empty trace_id
  let invalid_trace_ctx = SpanContext::new("", "b7ad6b7169203331", true, "")
  assert_false(SpanContext::is_valid(invalid_trace_ctx))
  
  // Test invalid span context with empty span_id
  let invalid_span_ctx = SpanContext::new("0af7651916cd43dd8448eb211c80319c", "", true, "")
  assert_false(SpanContext::is_valid(invalid_span_ctx))
  
  // Test unsampled span context
  let unsampled_ctx = SpanContext::new(valid_trace_id, valid_span_id, false, "")
  assert_true(SpanContext::is_valid(unsampled_ctx))
  assert_false(SpanContext::is_sampled(unsampled_ctx))
}

test "span hierarchy and relationships" {
  // Create parent span
  let parent_ctx = SpanContext::new("trace-123", "parent-span", true, "")
  let parent_span = Span::new("parent-operation", Server, parent_ctx)
  
  // Create child spans
  let child1_ctx = SpanContext::new("trace-123", "child-span-1", true, "")
  let child1_span = Span::new("child-operation-1", Internal, child1_ctx)
  
  let child2_ctx = SpanContext::new("trace-123", "child-span-2", true, "")
  let child2_span = Span::new("child-operation-2", Client, child2_ctx)
  
  // Verify all spans have the same trace ID
  assert_eq(SpanContext::trace_id(Span::span_context(parent_span)), "trace-123")
  assert_eq(SpanContext::trace_id(Span::span_context(child1_span)), "trace-123")
  assert_eq(SpanContext::trace_id(Span::span_context(child2_span)), "trace-123")
  
  // Verify different span IDs
  assert_eq(SpanContext::span_id(Span::span_context(parent_span)), "parent-span")
  assert_eq(SpanContext::span_id(Span::span_context(child1_span)), "child-span-1")
  assert_eq(SpanContext::span_id(Span::span_context(child2_span)), "child-span-2")
  
  // Verify all spans are valid and sampled
  assert_true(SpanContext::is_valid(Span::span_context(parent_span)))
  assert_true(SpanContext::is_valid(Span::span_context(child1_span)))
  assert_true(SpanContext::is_valid(Span::span_context(child2_span)))
  
  assert_true(SpanContext::is_sampled(Span::span_context(parent_span)))
  assert_true(SpanContext::is_sampled(Span::span_context(child1_span)))
  assert_true(SpanContext::is_sampled(Span::span_context(child2_span)))
}

test "span with complex attributes and events" {
  let span_ctx = SpanContext::new("trace-123", "span-456", true, "")
  let span = Span::new("complex-operation", Internal, span_ctx)
  
  // Add complex events with various attribute types
  let string_attrs = [("user.id", StringValue("user-123")), ("operation.name", StringValue("data.processing"))]
  Span::add_event(span, "user.action", Some(string_attrs))
  
  let numeric_attrs = [("request.size", IntValue(1024)), ("processing.time", FloatValue(150.5)), ("success.rate", FloatValue(0.95))]
  Span::add_event(span, "performance.metrics", Some(numeric_attrs))
  
  let boolean_attrs = [("cache.hit", BoolValue(true)), ("database.available", BoolValue(false))]
  Span::add_event(span, "system.status", Some(boolean_attrs))
  
  let array_attrs = [
    ("processed.items", ArrayIntValue([1, 2, 3, 4, 5])),
    ("tags", ArrayStringValue(["tag1", "tag2", "tag3"]))
  ]
  Span::add_event(span, "batch.processing", Some(array_attrs))
  
  // Update span status multiple times
  Span::set_status(span, Ok, Some("Processing started"))
  Span::set_status(span, Ok, Some("Processing in progress"))
  Span::set_status(span, Ok, Some("Processing completed successfully"))
  
  // Verify span is still in good state
  assert_true(Span::is_recording(span))
  assert_eq(Span::name(span), "complex-operation")
  assert_eq(Span::status(span), Ok)
}

test "span error handling and recovery" {
  let span_ctx = SpanContext::new("trace-123", "span-456", true, "")
  let span = Span::new("error-prone-operation", Internal, span_ctx)
  
  // Simulate error scenarios
  Span::add_event(span, "operation.started", None)
  
  // Record an error event
  let error_attrs = [
    ("error.type", StringValue("TimeoutError")),
    ("error.message", StringValue("Operation timed out after 30 seconds")),
    ("retry.attempt", IntValue(3)),
    ("max.retries", IntValue(5))
  ]
  Span::add_event(span, "error.occurred", Some(error_attrs))
  
  // Set error status
  Span::set_status(span, Error, Some("Operation failed due to timeout"))
  
  // Record recovery attempt
  let recovery_attrs = [
    ("recovery.action", StringValue("retry.with.backoff")),
    ("backoff.delay", FloatValue(2.5)),
    ("recovery.success", BoolValue(true))
  ]
  Span::add_event(span, "recovery.attempted", Some(recovery_attrs))
  
  // Update status after recovery
  Span::set_status(span, Ok, Some("Operation recovered and completed"))
  
  // Verify final state
  assert_eq(Span::status(span), Ok)
  assert_true(Span::is_recording(span))
}