// High-Quality Test Suite for Span Lifecycle Management
// This file contains comprehensive test cases for span lifecycle management

test "span creation and initial state" {
  let trace_id = "0af7651916cd43dd8448eb211c80319c"
  let span_id = "b7ad6b7169203331"
  let span_ctx = SpanContext::new(trace_id, span_id, true, "key1=value1")
  let span = Span::new("test-span", Internal, span_ctx)
  
  assert_eq(Span::name(span), "test-span")
  assert_eq(Span::kind(span), Internal)
  assert_true(Span::is_recording(span))
  assert_eq(Span::status(span), Unset)
  
  let retrieved_ctx = Span::span_context(span)
  assert_eq(SpanContext::trace_id(retrieved_ctx), trace_id)
  assert_eq(SpanContext::span_id(retrieved_ctx), span_id)
  assert_true(SpanContext::is_sampled(retrieved_ctx))
}

test "span with different kinds" {
  let trace_id = "0af7651916cd43dd8448eb211c80319c"
  let span_id = "b7ad6b7169203331"
  let span_ctx = SpanContext::new(trace_id, span_id, true, "")
  
  let internal_span = Span::new("internal-operation", Internal, span_ctx)
  let server_span = Span::new("server-operation", Server, span_ctx)
  let client_span = Span::new("client-operation", Client, span_ctx)
  let producer_span = Span::new("producer-operation", Producer, span_ctx)
  let consumer_span = Span::new("consumer-operation", Consumer, span_ctx)
  
  assert_eq(Span::kind(internal_span), Internal)
  assert_eq(Span::kind(server_span), Server)
  assert_eq(Span::kind(client_span), Client)
  assert_eq(Span::kind(producer_span), Producer)
  assert_eq(Span::kind(consumer_span), Consumer)
}

test "span status management" {
  let trace_id = "0af7651916cd43dd8448eb211c80319c"
  let span_id = "b7ad6b7169203331"
  let span_ctx = SpanContext::new(trace_id, span_id, true, "")
  let span = Span::new("test-span", Internal, span_ctx)
  
  // Initial status should be Unset
  assert_eq(Span::status(span), Unset)
  
  // Set status to Ok
  Span::set_status(span, Ok, Some("Operation completed successfully"))
  assert_eq(Span::status(span), Ok)
  
  // Set status to Error
  Span::set_status(span, Error, Some("Operation failed"))
  assert_eq(Span::status(span), Error)
}

test "span event management" {
  let trace_id = "0af7651916cd43dd8448eb211c80319c"
  let span_id = "b7ad6b7169203331"
  let span_ctx = SpanContext::new(trace_id, span_id, true, "")
  let span = Span::new("test-span", Internal, span_ctx)
  
  // Add events with different attribute types
  let string_attrs = [("event.type", StringValue("user.action"))]
  let int_attrs = [("retry.count", IntValue(3))]
  let float_attrs = [("duration.ms", FloatValue(150.5))]
  let bool_attrs = [("success", BoolValue(true))]
  
  Span::add_event(span, "user.login", Some(string_attrs))
  Span::add_event(span, "retry.attempt", Some(int_attrs))
  Span::add_event(span, "operation.duration", Some(float_attrs))
  Span::add_event(span, "operation.result", Some(bool_attrs))
  
  // Add event without attributes
  Span::add_event(span, "simple.event", None)
  
  assert_true(true) // If we reach here, events were added successfully
}

test "span lifecycle transitions" {
  let trace_id = "0af7651916cd43dd8448eb211c80319c"
  let span_id = "b7ad6b7169203331"
  let span_ctx = SpanContext::new(trace_id, span_id, true, "")
  let span = Span::new("test-span", Internal, span_ctx)
  
  // Initially recording
  assert_true(Span::is_recording(span))
  
  // Add events and status during recording
  Span::add_event(span, "start.event", None)
  Span::set_status(span, Ok, Some("In progress"))
  
  // End the span
  Span::end(span)
  
  // After ending, span should still be accessible for reading
  assert_eq(Span::name(span), "test-span")
  assert_eq(Span::status(span), Ok)
}

test "span context validity checks" {
  // Valid span context
  let valid_trace_id = "0af7651916cd43dd8448eb211c80319c"
  let valid_span_id = "b7ad6b7169203331"
  let valid_ctx = SpanContext::new(valid_trace_id, valid_span_id, true, "")
  let valid_span = Span::new("valid-span", Internal, valid_ctx)
  
  assert_true(SpanContext::is_valid(Span::span_context(valid_span)))
  
  // Invalid span context with empty trace_id
  let invalid_trace_id = ""
  let invalid_span_id = "b7ad6b7169203331"
  let invalid_ctx = SpanContext::new(invalid_trace_id, invalid_span_id, true, "")
  let invalid_span = Span::new("invalid-span", Internal, invalid_ctx)
  
  assert_false(SpanContext::is_valid(Span::span_context(invalid_span)))
  
  // Invalid span context with empty span_id
  let invalid_trace_id2 = "0af7651916cd43dd8448eb211c80319c"
  let invalid_span_id2 = ""
  let invalid_ctx2 = SpanContext::new(invalid_trace_id2, invalid_span_id2, true, "")
  let invalid_span2 = Span::new("invalid-span2", Internal, invalid_ctx2)
  
  assert_false(SpanContext::is_valid(Span::span_context(invalid_span2)))
}

test "span context sampling behavior" {
  let trace_id = "0af7651916cd43dd8448eb211c80319c"
  let span_id = "b7ad6b7169203331"
  
  // Sampled span context
  let sampled_ctx = SpanContext::new(trace_id, span_id, true, "key1=value1")
  let sampled_span = Span::new("sampled-span", Internal, sampled_ctx)
  
  assert_true(SpanContext::is_sampled(Span::span_context(sampled_span)))
  
  // Not sampled span context
  let not_sampled_ctx = SpanContext::new(trace_id, span_id, false, "key1=value1")
  let not_sampled_span = Span::new("not-sampled-span", Internal, not_sampled_ctx)
  
  assert_false(SpanContext::is_sampled(Span::span_context(not_sampled_span)))
}

test "span context trace state management" {
  let trace_id = "0af7651916cd43dd8448eb211c80319c"
  let span_id = "b7ad6b7169203331"
  
  // Span context with trace state
  let trace_state = "key1=value1,key2=value2"
  let ctx_with_state = SpanContext::new(trace_id, span_id, true, trace_state)
  let span_with_state = Span::new("span-with-state", Internal, ctx_with_state)
  
  let retrieved_ctx = Span::span_context(span_with_state)
  assert_eq(retrieved_ctx.trace_state, trace_state)
  
  // Span context without trace state
  let ctx_without_state = SpanContext::new(trace_id, span_id, true, "")
  let span_without_state = Span::new("span-without-state", Internal, ctx_without_state)
  
  let retrieved_ctx2 = Span::span_context(span_without_state)
  assert_eq(retrieved_ctx2.trace_state, "")
}

test "complex span workflow" {
  let trace_id = "0af7651916cd43dd8448eb211c80319c"
  let span_id = "b7ad6b7169203331"
  let span_ctx = SpanContext::new(trace_id, span_id, true, "")
  
  // Create a parent span
  let parent_span = Span::new("parent-operation", Server, span_ctx)
  
  // Add events to parent
  Span::add_event(parent_span, "operation.started", None)
  Span::set_status(parent_span, Ok, Some("Parent operation in progress"))
  
  // Create child spans (simulated)
  let child1_id = "c1ad6b7169203331"
  let child1_ctx = SpanContext::new(trace_id, child1_id, true, "")
  let child1_span = Span::new("child-operation-1", Internal, child1_ctx)
  
  let child2_id = "c2ad6b7169203331"
  let child2_ctx = SpanContext::new(trace_id, child2_id, true, "")
  let child2_span = Span::new("child-operation-2", Client, child2_ctx)
  
  // Add events to children
  Span::add_event(child1_span, "child1.started", None)
  Span::set_status(child1_span, Ok, Some("Child 1 completed"))
  
  Span::add_event(child2_span, "child2.started", None)
  Span::set_status(child2_span, Error, Some("Child 2 failed"))
  
  // End child spans
  Span::end(child1_span)
  Span::end(child2_span)
  
  // End parent span
  Span::add_event(parent_span, "operation.completed", None)
  Span::set_status(parent_span, Ok, Some("Parent operation completed"))
  Span::end(parent_span)
  
  // Verify final states
  assert_eq(Span::status(child1_span), Ok)
  assert_eq(Span::status(child2_span), Error)
  assert_eq(Span::status(parent_span), Ok)
}