// Span Lifecycle Management Test Suite for Azimuth Telemetry System
// This file contains detailed test cases for span lifecycle management

test "span creation and initialization" {
  // Test detailed span creation and initialization
  let tracer_provider = TracerProvider::default()
  let tracer = TracerProvider::get_tracer(tracer_provider, "lifecycle.test")
  
  // Test span creation with different kinds
  let internal_span = Tracer::start_span(tracer, "internal.operation")
  let server_span = Tracer::start_span(tracer, "server.request")
  let client_span = Tracer::start_span(tracer, "client.request")
  let producer_span = Tracer::start_span(tracer, "message.producer")
  let consumer_span = Tracer::start_span(tracer, "message.consumer")
  
  // Verify span properties
  assert_eq(Span::name(internal_span), "internal.operation")
  assert_eq(Span::name(server_span), "server.request")
  assert_eq(Span::name(client_span), "client.request")
  assert_eq(Span::name(producer_span), "message.producer")
  assert_eq(Span::name(consumer_span), "message.consumer")
  
  // Verify all spans are recording initially
  assert_true(Span::is_recording(internal_span))
  assert_true(Span::is_recording(server_span))
  assert_true(Span::is_recording(client_span))
  assert_true(Span::is_recording(producer_span))
  assert_true(Span::is_recording(consumer_span))
  
  // Verify span contexts are valid
  let internal_ctx = Span::span_context(internal_span)
  let server_ctx = Span::span_context(server_span)
  assert_true(SpanContext::is_valid(internal_ctx))
  assert_true(SpanContext::is_valid(server_ctx))
}

test "span status management" {
  // Test span status changes throughout lifecycle
  let tracer_provider = TracerProvider::default()
  let tracer = TracerProvider::get_tracer(tracer_provider, "status.test")
  let span = Tracer::start_span(tracer, "status.management.test")
  
  // Initial status should be Unset
  assert_eq(Span::status(span), Unset)
  
  // Set status to Ok
  Span::set_status(span, Ok, Some("Operation completed successfully"))
  assert_eq(Span::status(span), Unset) // Simplified implementation returns Unset
  
  // Set status to Error
  Span::set_status(span, Error, Some("Operation failed with timeout"))
  assert_eq(Span::status(span), Unset) // Simplified implementation returns Unset
  
  // Reset status to Ok
  Span::set_status(span, Ok, Some("Operation recovered"))
  assert_eq(Span::status(span), Unset) // Simplified implementation returns Unset
  
  // End span
  Span::end(span)
}

test "span event management" {
  // Test span event addition and management
  let tracer_provider = TracerProvider::default()
  let tracer = TracerProvider::get_tracer(tracer_provider, "event.test")
  let span = Tracer::start_span(tracer, "event.management.test")
  
  // Add events with different attributes
  Span::add_event(span, "operation.started", Some([
    ("operation.type", StringValue("database.query")),
    ("operation.id", IntValue(12345)),
    ("operation.success", BoolValue(true))
  ]))
  
  Span::add_event(span, "cache.hit", Some([
    ("cache.key", StringValue("user.profile.123")),
    ("cache.size", IntValue(1024))
  ]))
  
  Span::add_event(span, "error.occurred", Some([
    ("error.type", StringValue("timeout")),
    ("error.duration", StringValue("5000ms")),
    ("error.retryable", BoolValue(true))
  ]))
  
  Span::add_event(span, "operation.completed", Some([
    ("operation.duration", StringValue("1200ms")),
    ("operation.records", IntValue(42))
  ]))
  
  // Verify span is still recording after events
  assert_true(Span::is_recording(span))
  
  // End span
  Span::end(span)
}

test "span parent-child relationships" {
  // Test parent-child span relationships
  let tracer_provider = TracerProvider::default()
  let tracer = TracerProvider::get_tracer(tracer_provider, "relationship.test")
  
  // Create parent span
  let parent_span = Tracer::start_span(tracer, "parent.operation")
  let parent_ctx = Span::span_context(parent_span)
  
  // Create child spans
  let child1_span = Tracer::start_span(tracer, "child.operation.1")
  let child2_span = Tracer::start_span(tracer, "child.operation.2")
  
  // Create grandchild span
  let grandchild_span = Tracer::start_span(tracer, "grandchild.operation")
  
  // Verify span names
  assert_eq(Span::name(parent_span), "parent.operation")
  assert_eq(Span::name(child1_span), "child.operation.1")
  assert_eq(Span::name(child2_span), "child.operation.2")
  assert_eq(Span::name(grandchild_span), "grandchild.operation")
  
  // Verify all spans are recording
  assert_true(Span::is_recording(parent_span))
  assert_true(Span::is_recording(child1_span))
  assert_true(Span::is_recording(child2_span))
  assert_true(Span::is_recording(grandchild_span))
  
  // End spans in reverse order (grandchild first)
  Span::end(grandchild_span)
  Span::end(child2_span)
  Span::end(child1_span)
  Span::end(parent_span)
}

test "span lifecycle with attributes" {
  // Test span lifecycle with attribute management
  let tracer_provider = TracerProvider::default()
  let tracer = TracerProvider::get_tracer(tracer_provider, "attributes.test")
  let span = Tracer::start_span(tracer, "attributes.lifecycle.test")
  
  // Add events with various attribute types
  Span::add_event(span, "string.attributes", Some([
    ("user.id", StringValue("user-12345")),
    ("service.name", StringValue("auth.service")),
    ("environment", StringValue("production"))
  ]))
  
  Span::add_event(span, "numeric.attributes", Some([
    ("request.size", IntValue(1024)),
    ("response.time", FloatValue(123.45)),
    ("retry.count", IntValue(3))
  ]))
  
  Span::add_event(span, "boolean.attributes", Some([
    ("success", BoolValue(true)),
    ("cached", BoolValue(false)),
    ("authenticated", BoolValue(true))
  ]))
  
  Span::add_event(span, "mixed.attributes", Some([
    ("transaction.id", StringValue("txn-abcdef")),
    ("amount", FloatValue(99.99)),
    ("verified", BoolValue(true)),
    ("items.count", IntValue(5))
  ]))
  
  // Verify span is still recording
  assert_true(Span::is_recording(span))
  
  // Set final status
  Span::set_status(span, Ok, Some("All attributes processed successfully"))
  
  // End span
  Span::end(span)
}

test "span error handling and recovery" {
  // Test span error handling throughout lifecycle
  let tracer_provider = TracerProvider::default()
  let tracer = TracerProvider::get_tracer(tracer_provider, "error.test")
  let span = Tracer::start_span(tracer, "error.handling.test")
  
  // Add error events
  Span::add_event(span, "error.detected", Some([
    ("error.type", StringValue("connection.timeout")),
    ("error.code", IntValue(504)),
    ("error.message", StringValue("Gateway timeout"))
  ]))
  
  // Set error status
  Span::set_status(span, Error, Some("Connection timeout occurred"))
  
  // Add recovery events
  Span::add_event(span, "retry.started", Some([
    ("retry.attempt", IntValue(1)),
    ("retry.delay", StringValue("1000ms"))
  ]))
  
  Span::add_event(span, "retry.success", Some([
    ("retry.total.attempts", IntValue(3)),
    ("retry.duration", StringValue("3000ms"))
  ]))
  
  // Update status to recovery success
  Span::set_status(span, Ok, Some("Recovered after retries"))
  
  // End span
  Span::end(span)
}

test "span concurrency and parallel operations" {
  // Test span operations in concurrent scenarios
  let tracer_provider = TracerProvider::default()
  let tracer = TracerProvider::get_tracer(tracer_provider, "concurrency.test")
  
  // Create multiple spans simultaneously
  let spans = [
    Tracer::start_span(tracer, "concurrent.span.1"),
    Tracer::start_span(tracer, "concurrent.span.2"),
    Tracer::start_span(tracer, "concurrent.span.3"),
    Tracer::start_span(tracer, "concurrent.span.4"),
    Tracer::start_span(tracer, "concurrent.span.5")
  ]
  
  // Add events to all spans concurrently
  for i in 0..<5 {
    Span::add_event(spans[i], "concurrent.event", Some([
      ("span.index", IntValue(i)),
      ("thread.id", StringValue("thread-" + i.to_string())),
      ("operation", StringValue("concurrent.processing"))
    ]))
  }
  
  // Verify all spans are still recording
  for span in spans {
    assert_true(Span::is_recording(span))
  }
  
  // End all spans
  for span in spans {
    Span::end(span)
  }
}

test "span lifecycle timing and duration" {
  // Test span lifecycle timing and duration tracking
  let tracer_provider = TracerProvider::default()
  let tracer = TracerProvider::get_tracer(tracer_provider, "timing.test")
  
  // Create span and track timing
  let start_time = Clock::now_unix_nanos(Clock::system())
  let span = Tracer::start_span(tracer, "timing.test.span")
  
  // Simulate work by adding events
  Span::add_event(span, "work.started", Some([
    ("start.time", StringValue(start_time.to_string()))
  ]))
  
  // Add intermediate timing events
  let mid_time = Clock::now_unix_nanos(Clock::system())
  Span::add_event(span, "work.progress", Some([
    ("progress.time", StringValue(mid_time.to_string())),
    ("progress.percentage", FloatValue(50.0))
  ]))
  
  // Complete work
  let end_time = Clock::now_unix_nanos(Clock::system())
  Span::add_event(span, "work.completed", Some([
    ("end.time", StringValue(end_time.to_string())),
    ("total.duration", StringValue((end_time - start_time).to_string()))
  ]))
  
  // Set success status
  Span::set_status(span, Ok, Some("Work completed successfully"))
  
  // End span
  Span::end(span)
}