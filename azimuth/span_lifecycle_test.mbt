// Span Lifecycle Management Test Suite for Azimuth Telemetry System
// This file contains test cases for span creation, management, and lifecycle operations

test "span creation with different kinds" {
  let tracer_provider = TracerProvider::default()
  let tracer = TracerProvider::get_tracer(tracer_provider, "lifecycle.test")
  
  // Create spans with different kinds
  let internal_span = Tracer::start_span(tracer, "internal.operation")
  let server_span = Tracer::start_span(tracer, "server.request")
  let client_span = Tracer::start_span(tracer, "client.request")
  let producer_span = Tracer::start_span(tracer, "message.producer")
  let consumer_span = Tracer::start_span(tracer, "message.consumer")
  
  // Verify span names
  assert_eq(Span::name(internal_span), "internal.operation")
  assert_eq(Span::name(server_span), "server.request")
  assert_eq(Span::name(client_span), "client.request")
  assert_eq(Span::name(producer_span), "message.producer")
  assert_eq(Span::name(consumer_span), "message.consumer")
  
  // Verify all spans are recording
  assert_true(Span::is_recording(internal_span))
  assert_true(Span::is_recording(server_span))
  assert_true(Span::is_recording(client_span))
  assert_true(Span::is_recording(producer_span))
  assert_true(Span::is_recording(consumer_span))
}

test "span status management" {
  let tracer_provider = TracerProvider::default()
  let tracer = TracerProvider::get_tracer(tracer_provider, "status.test")
  let span = Tracer::start_span(tracer, "status.test.span")
  
  // Test initial status
  assert_eq(Span::status(span), Unset)
  
  // Test setting error status
  Span::set_status(span, Error, Some("Operation failed due to timeout"))
  assert_eq(Span::status(span), Error)  // Simplified implementation returns Unset
  
  // Test setting ok status
  Span::set_status(span, Ok, Some("Operation completed successfully"))
  assert_eq(Span::status(span), Ok)  // Simplified implementation returns Unset
  
  // Test setting unset status
  Span::set_status(span, Unset, None)
  assert_eq(Span::status(span), Unset)
}

test "span event management" {
  let tracer_provider = TracerProvider::default()
  let tracer = TracerProvider::get_tracer(tracer_provider, "events.test")
  let span = Tracer::start_span(tracer, "events.test.span")
  
  // Test adding events with different attribute types
  Span::add_event(span, "operation.started", Some([
    ("operation.type", StringValue("database.query")),
    ("timestamp", IntValue(1640995200)),
    ("success", BoolValue(true))
  ]))
  
  Span::add_event(span, "cache.hit", Some([
    ("cache.key", StringValue("user:12345")),
    ("cache.size", IntValue(1024)),
    ("hit.ratio", FloatValue(0.95))
  ]))
  
  Span::add_event(span, "operation.completed", Some([
    ("duration.ms", IntValue(150)),
    ("records.affected", IntValue(42)),
    ("transaction.id", StringValue("txn-abc123"))
  ]))
  
  // Test adding events without attributes
  Span::add_event(span, "simple.event", None)
  Span::add_event(span, "another.simple.event", None)
  
  // Verify span is still recording after events
  assert_true(Span::is_recording(span))
}

test "span context operations" {
  // Create span contexts with different configurations
  let sampled_ctx = SpanContext::new("0af7651916cd43dd8448eb211c80319c", "b7ad6b7169203331", true, "key1=value1")
  let not_sampled_ctx = SpanContext::new("0af7651916cd43dd8448eb211c80319c", "b7ad6b7169203332", false, "")
  let invalid_ctx = SpanContext::new("", "", false, "")
  
  // Test sampled context
  assert_eq(SpanContext::trace_id(sampled_ctx), "0af7651916cd43dd8448eb211c80319c")
  assert_eq(SpanContext::span_id(sampled_ctx), "b7ad6b7169203331")
  assert_true(SpanContext::is_sampled(sampled_ctx))
  assert_true(SpanContext::is_valid(sampled_ctx))
  
  // Test not sampled context
  assert_eq(SpanContext::trace_id(not_sampled_ctx), "0af7651916cd43dd8448eb211c80319c")
  assert_eq(SpanContext::span_id(not_sampled_ctx), "b7ad6b7169203332")
  assert_false(SpanContext::is_sampled(not_sampled_ctx))
  assert_true(SpanContext::is_valid(not_sampled_ctx))
  
  // Test invalid context
  assert_eq(SpanContext::trace_id(invalid_ctx), "")
  assert_eq(SpanContext::span_id(invalid_ctx), "")
  assert_false(SpanContext::is_sampled(invalid_ctx))
  assert_false(SpanContext::is_valid(invalid_ctx))
}

test "span lifecycle with parent-child relationships" {
  let tracer_provider = TracerProvider::default()
  let tracer = TracerProvider::get_tracer(tracer_provider, "hierarchy.test")
  
  // Create parent span
  let parent_span = Tracer::start_span(tracer, "parent.operation")
  
  // Add parent events
  Span::add_event(parent_span, "parent.started", Some([
    ("operation.type", StringValue("parent"))
  ]))
  
  // Create child spans (simplified - in real implementation would use parent context)
  let child_span1 = Tracer::start_span(tracer, "child.operation.1")
  let child_span2 = Tracer::start_span(tracer, "child.operation.2")
  
  // Add child events
  Span::add_event(child_span1, "child1.started", Some([
    ("parent.id", StringValue("parent.operation"))
  ]))
  
  Span::add_event(child_span2, "child2.started", Some([
    ("parent.id", StringValue("parent.operation"))
  ]))
  
  // End child spans
  Span::end(child_span1)
  Span::end(child_span2)
  
  // Add parent completion event
  Span::add_event(parent_span, "parent.children.completed", Some([
    ("children.count", IntValue(2))
  ]))
  
  // End parent span
  Span::end(parent_span)
  
  // Verify span names
  assert_eq(Span::name(parent_span), "parent.operation")
  assert_eq(Span::name(child_span1), "child.operation.1")
  assert_eq(Span::name(child_span2), "child.operation.2")
}

test "span termination and cleanup" {
  let tracer_provider = TracerProvider::default()
  let tracer = TracerProvider::get_tracer(tracer_provider, "termination.test")
  let span = Tracer::start_span(tracer, "termination.test.span")
  
  // Verify span is initially recording
  assert_true(Span::is_recording(span))
  
  // Add events before termination
  Span::add_event(span, "pre.termination.event", Some([
    ("phase", StringValue("active"))
  ]))
  
  // Set final status
  Span::set_status(span, Ok, Some("Operation completed successfully"))
  
  // Add final event
  Span::add_event(span, "termination.event", Some([
    ("phase", StringValue("terminating")),
    ("final.status", StringValue("success"))
  ]))
  
  // End the span
  Span::end(span)
  
  // Verify span properties after ending
  assert_eq(Span::name(span), "termination.test.span")
  assert_eq(Span::status(span), Ok)  // Simplified implementation returns Unset
}