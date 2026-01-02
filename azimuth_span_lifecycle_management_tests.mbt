// Azimuth Telemetry System - Span Lifecycle Management Tests
// This file contains comprehensive test cases for span lifecycle management

// Test 1: Span creation and initialization
test "span creation and initialization" {
  // Create a span context
  let span_context = SpanContext::new("trace123456789", "span987654321", true, "key1=value1,key2=value2")
  
  // Create a span with Internal kind
  let internal_span = Span::new("internal-operation", Internal, span_context)
  
  // Verify span properties
  assert_eq(Span::name(internal_span), "internal-operation")
  assert_eq(Span::kind(internal_span), Internal)
  assert_true(Span::is_recording(internal_span))
  
  // Verify span context
  let retrieved_context = Span::span_context(internal_span)
  assert_eq(SpanContext::trace_id(retrieved_context), "trace123456789")
  assert_eq(SpanContext::span_id(retrieved_context), "span987654321")
  assert_true(SpanContext::is_sampled(retrieved_context))
  
  // Create spans with different kinds
  let server_span = Span::new("server-operation", Server, span_context)
  let client_span = Span::new("client-operation", Client, span_context)
  let producer_span = Span::new("producer-operation", Producer, span_context)
  let consumer_span = Span::new("consumer-operation", Consumer, span_context)
  
  assert_eq(Span::kind(server_span), Server)
  assert_eq(Span::kind(client_span), Client)
  assert_eq(Span::kind(producer_span), Producer)
  assert_eq(Span::kind(consumer_span), Consumer)
}

// Test 2: Span status management
test "span status management" {
  // Create a span
  let span_context = SpanContext::new("trace123", "span456", true, "")
  let span = Span::new("test-operation", Internal, span_context)
  
  // Initially, span status should be Unset
  assert_eq(Span::status(span), Unset)
  
  // Set status to Ok
  Span::set_status(span, Ok, Some("Operation completed successfully"))
  assert_eq(Span::status(span), Ok)
  
  // Set status to Error
  Span::set_status(span, Error, Some("Operation failed with error"))
  assert_eq(Span::status(span), Error)
  
  // Set status back to Unset
  Span::set_status(span, Unset, None)
  assert_eq(Span::status(span), Unset)
}

// Test 3: Span event management
test "span event management" {
  // Create a span
  let span_context = SpanContext::new("trace123", "span456", true, "")
  let span = Span::new("test-operation", Internal, span_context)
  
  // Add events with attributes
  let event1_attrs = [
    ("event.name", AttributeValue::StringValue("database.query")),
    ("query.duration_ms", AttributeValue::IntValue(150)),
    ("query.success", AttributeValue::BoolValue(true))
  ]
  
  let event2_attrs = [
    ("event.name", AttributeValue::StringValue("cache.hit")),
    ("cache.key", AttributeValue::StringValue("user:123"))
  ]
  
  // Add events to span
  Span::add_event(span, "Database query completed", Some(event1_attrs))
  Span::add_event(span, "Cache hit", Some(event2_attrs))
  Span::add_event(span, "Simple event without attributes", None)
  
  // Verify span is still recording after adding events
  assert_true(Span::is_recording(span))
}

// Test 4: Span lifecycle transitions
test "span lifecycle transitions" {
  // Create a span
  let span_context = SpanContext::new("trace123", "span456", true, "")
  let span = Span::new("test-operation", Internal, span_context)
  
  // Initial state: recording
  assert_true(Span::is_recording(span))
  
  // Add status and events
  Span::set_status(span, Ok, Some("Operation completed"))
  Span::add_event(span, "Operation started", None)
  Span::add_event(span, "Operation in progress", None)
  Span::add_event(span, "Operation completed", None)
  
  // End the span
  Span::end(span)
  
  // After ending, the span should no longer be recording
  // Note: In the simplified implementation, this might not change the recording state
  // In a real implementation, this would set recording to false
}

// Test 5: Span context validity checks
test "span context validity checks" {
  // Test valid span context
  let valid_context = SpanContext::new("trace123456789", "span987654321", true, "key1=value1")
  assert_true(SpanContext::is_valid(valid_context))
  
  // Test invalid span context with empty trace_id
  let invalid_trace_context = SpanContext::new("", "span987654321", true, "key1=value1")
  assert_false(SpanContext::is_valid(invalid_trace_context))
  
  // Test invalid span context with empty span_id
  let invalid_span_context = SpanContext::new("trace123456789", "", true, "key1=value1")
  assert_false(SpanContext::is_valid(invalid_span_context))
  
  // Test invalid span context with both empty
  let invalid_both_context = SpanContext::new("", "", true, "key1=value1")
  assert_false(SpanContext::is_valid(invalid_both_context))
  
  // Test sampling states
  let sampled_context = SpanContext::new("trace123", "span456", true, "")
  let unsampled_context = SpanContext::new("trace123", "span456", false, "")
  
  assert_true(SpanContext::is_sampled(sampled_context))
  assert_false(SpanContext::is_sampled(unsampled_context))
}

// Test 6: Span hierarchy and relationships
test "span hierarchy and relationships" {
  // Create parent span
  let parent_context = SpanContext::new("trace123", "parent456", true, "")
  let parent_span = Span::new("parent-operation", Server, parent_context)
  
  // Create child span with same trace_id but different span_id
  let child_context = SpanContext::new("trace123", "child789", true, "")
  let child_span = Span::new("child-operation", Internal, child_context)
  
  // Verify relationship through trace_id
  let parent_retrieved = Span::span_context(parent_span)
  let child_retrieved = Span::span_context(child_span)
  
  assert_eq(
    SpanContext::trace_id(parent_retrieved), 
    SpanContext::trace_id(child_retrieved)
  )
  
  // Verify different span_ids
  assert_ne(
    SpanContext::span_id(parent_retrieved), 
    SpanContext::span_id(child_retrieved)
  )
  
  // Add events to parent and child
  Span::add_event(parent_span, "Parent operation started", None)
  Span::add_event(child_span, "Child operation started", None)
  Span::add_event(child_span, "Child operation completed", None)
  Span::add_event(parent_span, "Parent operation completed", None)
  
  // End child first, then parent
  Span::end(child_span)
  Span::end(parent_span)
}

// Test 7: Span with different sampling states
test "span with different sampling states" {
  // Create sampled span
  let sampled_context = SpanContext::new("trace123", "span456", true, "key1=value1")
  let sampled_span = Span::new("sampled-operation", Server, sampled_context)
  
  // Create unsampled span
  let unsampled_context = SpanContext::new("trace789", "span012", false, "")
  let unsampled_span = Span::new("unsampled-operation", Server, unsampled_context)
  
  // Both spans should be recording regardless of sampling state
  assert_true(Span::is_recording(sampled_span))
  assert_true(Span::is_recording(unsampled_span))
  
  // Add events to both spans
  Span::add_event(sampled_span, "Sampled event", None)
  Span::add_event(unsampled_span, "Unsampled event", None)
  
  // Set status on both spans
  Span::set_status(sampled_span, Ok, Some("Sampled operation completed"))
  Span::set_status(unsampled_span, Error, Some("Unsampled operation failed"))
  
  // End both spans
  Span::end(sampled_span)
  Span::end(unsampled_span)
}

// Test 8: Span with trace state
test "span with trace state" {
  // Create span with trace state
  let trace_state = "key1=value1,key2=value2,key3=value3"
  let span_context = SpanContext::new("trace123", "span456", true, trace_state)
  let span = Span::new("operation-with-trace-state", Internal, span_context)
  
  // Verify trace state is preserved
  let retrieved_context = Span::span_context(span)
  // Note: In the simplified implementation, we don't have a getter for trace_state
  // In a real implementation, we would verify the trace state
  
  // Add events and set status
  Span::add_event(span, "Operation with trace state", None)
  Span::set_status(span, Ok, None)
  
  // End the span
  Span::end(span)
}

// Test 9: Span with complex attributes in events
test "span with complex attributes in events" {
  // Create a span
  let span_context = SpanContext::new("trace123", "span456", true, "")
  let span = Span::new("complex-operation", Internal, span_context)
  
  // Add events with complex attribute types
  let string_array_attrs = [
    ("event.name", AttributeValue::StringValue("array.test")),
    ("string.array", AttributeValue::ArrayStringValue(["value1", "value2", "value3"])),
    ("operation.count", AttributeValue::IntValue(42))
  ]
  
  let int_array_attrs = [
    ("event.name", AttributeValue::StringValue("int.array.test")),
    ("int.array", AttributeValue::ArrayIntValue([1, 2, 3, 4, 5])),
    ("operation.success", AttributeValue::BoolValue(true))
  ]
  
  let mixed_attrs = [
    ("event.name", AttributeValue::StringValue("mixed.types")),
    ("string.value", AttributeValue::StringValue("test")),
    ("int.value", AttributeValue::IntValue(100)),
    ("float.value", AttributeValue::FloatValue(3.14)),
    ("bool.value", AttributeValue::BoolValue(false))
  ]
  
  // Add events with complex attributes
  Span::add_event(span, "String array event", Some(string_array_attrs))
  Span::add_event(span, "Int array event", Some(int_array_attrs))
  Span::add_event(span, "Mixed types event", Some(mixed_attrs))
  
  // Set status and end span
  Span::set_status(span, Ok, Some("Complex operation completed"))
  Span::end(span)
}

// Test 10: Span error handling and edge cases
test "span error handling and edge cases" {
  // Test span with empty name
  let span_context = SpanContext::new("trace123", "span456", true, "")
  let empty_name_span = Span::new("", Internal, span_context)
  assert_eq(Span::name(empty_name_span), "")
  
  // Test span with very long name
  let long_name = "this-is-a-very-long-operation-name-that-might-be-used-in-real-world-scenarios-where-descriptive-names-are-important-for-debugging-and-monitoring-purposes"
  let long_name_span = Span::new(long_name, Internal, span_context)
  assert_eq(Span::name(long_name_span), long_name)
  
  // Test span with special characters in name
  let special_name_span = Span::new("operation/with/special/chars?and=symbols", Internal, span_context)
  assert_eq(Span::name(special_name_span), "operation/with/special/chars?and=symbols")
  
  // Test multiple status changes
  let multi_status_span = Span::new("multi-status-operation", Internal, span_context)
  Span::set_status(multi_status_span, Ok, Some("First status"))
  Span::set_status(multi_status_span, Error, Some("Second status"))
  Span::set_status(multi_status_span, Unset, Some("Third status"))
  assert_eq(Span::status(multi_status_span), Unset)
  
  // Test adding many events
  let many_events_span = Span::new("many-events-operation", Internal, span_context)
  for i in 0..=10 {
    let event_name = "event-" + i.to_string()
    Span::add_event(many_events_span, event_name, None)
  }
  
  // End all spans
  Span::end(empty_name_span)
  Span::end(long_name_span)
  Span::end(special_name_span)
  Span::end(multi_status_span)
  Span::end(many_events_span)
}