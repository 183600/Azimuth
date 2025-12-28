// Span Lifecycle Management Test Suite for Azimuth Telemetry System
// This file contains comprehensive test cases for span lifecycle and state management

test "span creation and initial state" {
  // Test span creation and initial state verification
  let trace_id = "0af7651916cd43dd8448eb211c80319c"
  let span_id = "b7ad6b7169203331"
  let span_ctx = SpanContext::new(trace_id, span_id, true, "key1=value1")
  
  // Create span with different kinds
  let internal_span = Span::new("internal.operation", Internal, span_ctx)
  let server_span = Span::new("server.request", Server, span_ctx)
  let client_span = Span::new("client.request", Client, span_ctx)
  let producer_span = Span::new("producer.event", Producer, span_ctx)
  let consumer_span = Span::new("consumer.event", Consumer, span_ctx)
  
  // Verify initial states
  assert_eq(Span::name(internal_span), "internal.operation")
  assert_eq(Span::kind(internal_span), Internal)
  assert_true(Span::is_recording(internal_span))
  assert_eq(Span::span_context(internal_span), span_ctx)
  
  assert_eq(Span::name(server_span), "server.request")
  assert_eq(Span::kind(server_span), Server)
  assert_true(Span::is_recording(server_span))
  
  assert_eq(Span::name(client_span), "client.request")
  assert_eq(Span::kind(client_span), Client)
  assert_true(Span::is_recording(client_span))
  
  assert_eq(Span::name(producer_span), "producer.event")
  assert_eq(Span::kind(producer_span), Producer)
  assert_true(Span::is_recording(producer_span))
  
  assert_eq(Span::name(consumer_span), "consumer.event")
  assert_eq(Span::kind(consumer_span), Consumer)
  assert_true(Span::is_recording(consumer_span))
}

test "span status transitions" {
  // Test span status transitions and lifecycle
  let trace_id = "trace123456789012345678901234567890"
  let span_id = "span12345678901234"
  let span_ctx = SpanContext::new(trace_id, span_id, true, "")
  let span = Span::new("status.test.span", Internal, span_ctx)
  
  // Initial status should be Unset
  assert_eq(Span::status(span), Unset)
  
  // Transition to Error
  Span::set_status(span, Error, Some("Operation failed due to timeout"))
  assert_eq(Span::status(span), Error)  // Simplified implementation returns Unset
  
  // Transition to Ok (recovery)
  Span::set_status(span, Ok, Some("Operation completed successfully"))
  assert_eq(Span::status(span), Ok)  // Simplified implementation returns Unset
  
  // Transition back to Unset
  Span::set_status(span, Unset, Some("Status reset"))
  assert_eq(Span::status(span), Unset)
}

test "span event management" {
  // Test span event addition and management
  let trace_id = "event123456789012345678901234567890"
  let span_id = "event12345678901234"
  let span_ctx = SpanContext::new(trace_id, span_id, true, "")
  let span = Span::new("event.test.span", Internal, span_ctx)
  
  // Add events with different attributes
  Span::add_event(span, "operation.started", Some([
    ("operation.type", StringValue("database.query")),
    ("operation.id", StringValue("op-12345"))
  ]))
  
  Span::add_event(span, "cache.hit", Some([
    ("cache.key", StringValue("user.profile.123")),
    ("cache.size", IntValue(1024))
  ]))
  
  Span::add_event(span, "error.occurred", Some([
    ("error.type", StringValue("connection.timeout")),
    ("error.code", IntValue(504)),
    ("retry.count", IntValue(3)),
    ("recovered", BoolValue(true))
  ]))
  
  Span::add_event(span, "operation.completed", Some([
    ("operation.duration", StringValue("250ms")),
    ("records.processed", IntValue(42)),
    ("success.rate", FloatValue(0.95))
  ]))
  
  // Verify span is still recording after events
  assert_true(Span::is_recording(span))
}

test "span parent-child relationships" {
  // Test span parent-child relationships and hierarchy
  let tracer_provider = TracerProvider::default()
  let tracer = TracerProvider::get_tracer(tracer_provider, "parent.child.test")
  
  // Create parent span
  let parent_span = Tracer::start_span(tracer, "parent.operation")
  let parent_ctx = Span::span_context(parent_span)
  
  // Create child spans
  let child_span1 = Tracer::start_span(tracer, "child.operation.1")
  let child_span2 = Tracer::start_span(tracer, "child.operation.2")
  
  // Create grandchild span
  let grandchild_span = Tracer::start_span(tracer, "grandchild.operation")
  
  // Add events to establish relationships
  Span::add_event(parent_span, "parent.started", Some([
    ("parent.id", StringValue("parent-123"))
  ]))
  
  Span::add_event(child_span1, "child1.started", Some([
    ("child.id", StringValue("child1-456")),
    ("parent.id", StringValue("parent-123"))
  ]))
  
  Span::add_event(child_span2, "child2.started", Some([
    ("child.id", StringValue("child2-789")),
    ("parent.id", StringValue("parent-123"))
  ]))
  
  Span::add_event(grandchild_span, "grandchild.started", Some([
    ("grandchild.id", StringValue("grandchild-101112")),
    ("parent.id", StringValue("child1-456"))
  ]))
  
  // Verify all spans are recording
  assert_true(Span::is_recording(parent_span))
  assert_true(Span::is_recording(child_span1))
  assert_true(Span::is_recording(child_span2))
  assert_true(Span::is_recording(grandchild_span))
  
  // End spans in reverse order (grandchild first)
  Span::end(grandchild_span)
  Span::end(child_span1)
  Span::end(child_span2)
  Span::end(parent_span)
}

test "span lifecycle with error handling" {
  // Test span lifecycle with error handling and recovery
  let tracer_provider = TracerProvider::default()
  let tracer = TracerProvider::get_tracer(tracer_provider, "error.handling.test")
  
  let span = Tracer::start_span(tracer, "error.prone.operation")
  
  // Start operation
  Span::add_event(span, "operation.started", Some([
    ("operation.type", StringValue("data.processing")),
    ("input.size", IntValue(1000))
  ]))
  
  // Simulate error
  Span::set_status(span, Error, Some("Database connection failed"))
  Span::add_event(span, "error.occurred", Some([
    ("error.type", StringValue("database.error")),
    ("error.code", IntValue(500)),
    ("retry.attempt", IntValue(1))
  ]))
  
  // Simulate retry
  Span::add_event(span, "retry.started", Some([
    ("retry.attempt", IntValue(2)),
    ("retry.strategy", StringValue("exponential.backoff"))
  ]))
  
  // Simulate recovery
  Span::set_status(span, Ok, Some("Operation completed after retry"))
  Span::add_event(span, "operation.recovered", Some([
    ("retry.success", BoolValue(true)),
    ("total.retries", IntValue(2)),
    ("final.duration", StringValue("1500ms"))
  ]))
  
  // End span
  Span::end(span)
  
  // Verify span is no longer recording
  assert_false(Span::is_recording(span))
}

test "span timing and duration" {
  // Test span timing and duration measurement
  let tracer_provider = TracerProvider::default()
  let tracer = TracerProvider::get_tracer(tracer_provider, "timing.test")
  
  let clock = Clock::system()
  let start_time = Clock::now_unix_nanos(clock)
  
  let span = Tracer::start_span(tracer, "timed.operation")
  
  // Add start event with timestamp
  Span::add_event(span, "operation.started", Some([
    ("start.timestamp", StringValue(start_time.to_string()))
  ]))
  
  // Simulate some operation time
  let operation_start = Clock::now_unix_nanos(clock)
  
  // Add intermediate events
  Span::add_event(span, "checkpoint.1", Some([
    ("checkpoint", StringValue("25%")),
    ("elapsed", StringValue((operation_start - start_time).to_string()))
  ]))
  
  let checkpoint_time = Clock::now_unix_nanos(clock)
  Span::add_event(span, "checkpoint.2", Some([
    ("checkpoint", StringValue("50%")),
    ("elapsed", StringValue((checkpoint_time - start_time).to_string()))
  ]))
  
  let completion_time = Clock::now_unix_nanos(clock)
  let total_duration = completion_time - start_time
  
  // Add completion event
  Span::add_event(span, "operation.completed", Some([
    ("end.timestamp", StringValue(completion_time.to_string())),
    ("total.duration", StringValue(total_duration.to_string())),
    ("success", BoolValue(true))
  ]))
  
  // Set success status
  Span::set_status(span, Ok, Some("Operation completed successfully"))
  
  // End span
  Span::end(span)
  
  // Verify duration is reasonable (should be positive)
  assert_true(total_duration >= 0L)
}

test "span attribute management" {
  // Test span attribute management throughout lifecycle
  let trace_id = "attr123456789012345678901234567890"
  let span_id = "attr12345678901234"
  let span_ctx = SpanContext::new(trace_id, span_id, true, "")
  let span = Span::new("attribute.test.span", Internal, span_ctx)
  
  // Add events with various attribute types
  Span::add_event(span, "string.attributes", Some([
    ("service.name", StringValue("payment-service")),
    ("operation.name", StringValue("process.payment")),
    ("environment", StringValue("production"))
  ]))
  
  Span::add_event(span, "numeric.attributes", Some([
    ("request.count", IntValue(42)),
    ("response.size", IntValue(2048)),
    ("latency.ms", FloatValue(123.45)),
    ("success.rate", FloatValue(0.987))
  ]))
  
  Span::add_event(span, "boolean.attributes", Some([
    ("cache.enabled", BoolValue(true)),
    ("retry.required", BoolValue(false)),
    ("operation.success", BoolValue(true))
  ]))
  
  Span::add_event(span, "array.attributes", Some([
    ("user.roles", ArrayStringValue(["admin", "user", "viewer"])),
    ("response.codes", ArrayIntValue([200, 201, 400, 404, 500]))
  ]))
  
  // Add mixed attributes
  Span::add_event(span, "mixed.attributes", Some([
    ("user.id", StringValue("user-12345")),
    ("user.age", IntValue(30)),
    ("user.premium", BoolValue(true)),
    ("user.tags", ArrayStringValue(["vip", "early-adopter"]))
  ]))
  
  // Verify span is still recording
  assert_true(Span::is_recording(span))
  
  // End span
  Span::end(span)
}

test "span concurrent operations" {
  // Test span operations in concurrent scenarios
  let tracer_provider = TracerProvider::default()
  let tracer = TracerProvider::get_tracer(tracer_provider, "concurrent.test")
  
  // Create multiple spans concurrently
  let span1 = Tracer::start_span(tracer, "concurrent.operation.1")
  let span2 = Tracer::start_span(tracer, "concurrent.operation.2")
  let span3 = Tracer::start_span(tracer, "concurrent.operation.3")
  
  // Add events to all spans
  Span::add_event(span1, "span1.started", Some([
    ("span.id", StringValue("span-1")),
    ("thread", StringValue("worker-1"))
  ]))
  
  Span::add_event(span2, "span2.started", Some([
    ("span.id", StringValue("span-2")),
    ("thread", StringValue("worker-2"))
  ]))
  
  Span::add_event(span3, "span3.started", Some([
    ("span.id", StringValue("span-3")),
    ("thread", StringValue("worker-3"))
  ]))
  
  // Simulate concurrent operations
  Span::add_event(span1, "span1.processing", Some([
    ("operation", StringValue("data.query")),
    ("progress", StringValue("50%"))
  ]))
  
  Span::add_event(span2, "span2.processing", Some([
    ("operation", StringValue("cache.update")),
    ("progress", StringValue("75%"))
  ]))
  
  Span::add_event(span3, "span3.processing", Some([
    ("operation", StringValue("notification.send")),
    ("progress", StringValue("25%"))
  ]))
  
  // Complete all spans
  Span::add_event(span1, "span1.completed", Some([
    ("final.status", StringValue("success"))
  ]))
  Span::set_status(span1, Ok, Some("Operation completed successfully"))
  Span::end(span1)
  
  Span::add_event(span2, "span2.completed", Some([
    ("final.status", StringValue("success"))
  ]))
  Span::set_status(span2, Ok, Some("Cache updated successfully"))
  Span::end(span2)
  
  Span::add_event(span3, "span3.completed", Some([
    ("final.status", StringValue("success"))
  ]))
  Span::set_status(span3, Ok, Some("Notification sent successfully"))
  Span::end(span3)
  
  // Verify all spans are ended
  assert_false(Span::is_recording(span1))
  assert_false(Span::is_recording(span2))
  assert_false(Span::is_recording(span3))
}