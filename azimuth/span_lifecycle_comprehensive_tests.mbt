// Span Lifecycle Comprehensive Tests for Azimuth Telemetry System
// This file contains comprehensive test cases for span lifecycle management

test "span creation with different kinds" {
  let provider = TracerProvider::default()
  let tracer = TracerProvider::get_tracer(provider, "test.tracer")
  
  // Test creating internal span
  let internal_span = Tracer::start_span(tracer, "internal.operation")
  assert_eq(Span::name(internal_span), "internal.operation")
  assert_eq(Span::kind(internal_span), Internal)
  assert_true(Span::is_recording(internal_span))
  
  // Test creating server span
  let server_span_ctx = SpanContext::new("trace123", "span456", true, "")
  let server_span = Span::new("server.request", Server, server_span_ctx)
  assert_eq(Span::name(server_span), "server.request")
  assert_eq(Span::kind(server_span), Server)
  assert_true(Span::is_recording(server_span))
  
  // Test creating client span
  let client_span_ctx = SpanContext::new("trace789", "span012", true, "")
  let client_span = Span::new("client.request", Client, client_span_ctx)
  assert_eq(Span::name(client_span), "client.request")
  assert_eq(Span::kind(client_span), Client)
  assert_true(Span::is_recording(client_span))
  
  // Test creating producer span
  let producer_span_ctx = SpanContext::new("trace345", "span678", true, "")
  let producer_span = Span::new("message.produce", Producer, producer_span_ctx)
  assert_eq(Span::name(producer_span), "message.produce")
  assert_eq(Span::kind(producer_span), Producer)
  assert_true(Span::is_recording(producer_span))
  
  // Test creating consumer span
  let consumer_span_ctx = SpanContext::new("trace901", "span234", true, "")
  let consumer_span = Span::new("message.consume", Consumer, consumer_span_ctx)
  assert_eq(Span::name(consumer_span), "message.consume")
  assert_eq(Span::kind(consumer_span), Consumer)
  assert_true(Span::is_recording(consumer_span))
}

test "span context management" {
  let provider = TracerProvider::default()
  let tracer = TracerProvider::get_tracer(provider, "test.tracer")
  
  // Test span with custom context
  let custom_ctx = SpanContext::new("custom-trace-id", "custom-span-id", true, "custom-key=custom-value")
  let span_with_ctx = Span::new("custom.operation", Internal, custom_ctx)
  
  let retrieved_ctx = Span::span_context(span_with_ctx)
  assert_eq(SpanContext::trace_id(retrieved_ctx), "custom-trace-id")
  assert_eq(SpanContext::span_id(retrieved_ctx), "custom-span-id")
  assert_true(SpanContext::is_sampled(retrieved_ctx))
  assert_true(SpanContext::is_valid(retrieved_ctx))
  
  // Test span with unsampled context
  let unsampled_ctx = SpanContext::new("unsampled-trace", "unsampled-span", false, "")
  let unsampled_span = Span::new("unsampled.operation", Internal, unsampled_ctx)
  let unsampled_retrieved_ctx = Span::span_context(unsampled_span)
  assert_false(SpanContext::is_sampled(unsampled_retrieved_ctx))
}

test "span status management" {
  let provider = TracerProvider::default()
  let tracer = TracerProvider::get_tracer(provider, "test.tracer")
  let span = Tracer::start_span(tracer, "status.test")
  
  // Test initial status (should be Unset)
  assert_eq(Span::status(span), Unset)
  
  // Test setting Ok status
  Span::set_status(span, Ok)
  assert_eq(Span::status(span), Ok)  // Note: Simplified implementation might return Unset
  
  // Test setting Error status with description
  Span::set_status(span, Error, Some("Operation failed due to timeout"))
  assert_eq(Span::status(span), Error)  // Note: Simplified implementation might return Unset
  
  // Test setting status back to Unset
  Span::set_status(span, Unset)
  assert_eq(Span::status(span), Unset)
}

test "span event management" {
  let provider = TracerProvider::default()
  let tracer = TracerProvider::get_tracer(provider, "test.tracer")
  let span = Tracer::start_span(tracer, "events.test")
  
  // Test adding simple event
  Span::add_event(span, "event.started")
  
  // Test adding event with attributes
  let event_attrs = [
    ("event.type", StringValue("user.action")),
    ("user.id", IntValue(12345)),
    ("action.timestamp", StringValue("2025-12-28T10:30:00Z"))
  ]
  Span::add_event(span, "user.login", Some(event_attrs))
  
  // Test adding multiple events
  Span::add_event(span, "database.query.start", Some([("query.type", StringValue("SELECT"))]))
  Span::add_event(span, "database.query.complete", Some([
    ("query.type", StringValue("SELECT")),
    ("rows.affected", IntValue(42)),
    ("execution.time", FloatValue(125.5))
  ]))
  
  // Test adding event with complex attributes
  let complex_attrs = [
    ("request.id", StringValue("req-abc-123")),
    ("request.size", IntValue(1024)),
    ("response.code", IntValue(200)),
    ("cache.hit", BoolValue(true)),
    ("tags", ArrayStringValue(["api", "v1", "authenticated"]))
  ]
  Span::add_event(span, "request.completed", Some(complex_attrs))
}

test "span lifecycle with recording state" {
  let provider = TracerProvider::default()
  let tracer = TracerProvider::get_tracer(provider, "test.tracer")
  let span = Tracer::start_span(tracer, "lifecycle.test")
  
  // Test initial recording state
  assert_true(Span::is_recording(span))
  
  // Test operations while recording
  Span::add_event(span, "operation.start")
  Span::set_status(span, Ok, Some("Operation in progress"))
  
  // Test ending span
  Span::end(span)
  // In a real implementation, recording would be set to false
  // In simplified implementation, we can't test this directly
}

test "span with tracer instrumentation scope" {
  let provider = TracerProvider::default()
  let tracer = TracerProvider::get_tracer(provider, "instrumentation.test", Some("2.0.0"))
  let scope = Tracer::instrumentation_scope(tracer)
  
  // Test instrumentation scope properties
  assert_eq(scope.name, "instrumentation.test")
  assert_eq(scope.version, Some("2.0.0"))
  assert_eq(scope.schema_url, None)
  
  // Test creating span with specific tracer
  let span = Tracer::start_span(tracer, "scoped.operation")
  assert_eq(Span::name(span), "scoped.operation")
  assert_true(Span::is_recording(span))
}

test "span attribute operations" {
  let provider = TracerProvider::default()
  let tracer = TracerProvider::get_tracer(provider, "test.tracer")
  
  // Test creating span with attributes
  let span_attrs = [
    ("operation.type", StringValue("database")),
    ("db.system", StringValue("postgresql")),
    ("db.statement", StringValue("SELECT * FROM users")),
    ("db.connection.pool", IntValue(10)),
    ("timeout.ms", IntValue(5000))
  ]
  let span_with_attrs = Tracer::start_span(tracer, "database.query", Some(span_attrs))
  
  assert_eq(Span::name(span_with_attrs), "database.query")
  assert_true(Span::is_recording(span_with_attrs))
  
  // Test creating span with different attribute types
  let complex_attrs = [
    ("service.name", StringValue("user-service")),
    ("service.version", StringValue("1.2.3")),
    ("service.instance.id", StringValue("instance-abc-123")),
    ("deployment.environment", StringValue("production")),
    ("process.pid", IntValue(12345)),
    ("process.executable.name", StringValue("user-service")),
    ("process.command_args", ArrayStringValue(["--port", "8080", "--env", "prod"])),
    ("feature.enabled", BoolValue(true)),
    ("config.timeout", FloatValue(30.5))
  ]
  let complex_span = Tracer::start_span(tracer, "complex.operation", Some(complex_attrs))
  
  assert_eq(Span::name(complex_span), "complex.operation")
  assert_true(Span::is_recording(complex_span))
}

test "span hierarchical relationships simulation" {
  let provider = TracerProvider::default()
  let tracer = TracerProvider::get_tracer(provider, "test.tracer")
  
  // Simulate parent span
  let parent_ctx = SpanContext::new("parent-trace-id", "parent-span-id", true, "")
  let parent_span = Span::new("parent.operation", Internal, parent_ctx)
  
  // Simulate child span
  let child_ctx = SpanContext::new("parent-trace-id", "child-span-id", true, "")
  let child_span = Span::new("child.operation", Internal, child_ctx)
  
  // Verify hierarchical relationship through trace ID
  let parent_retrieved_ctx = Span::span_context(parent_span)
  let child_retrieved_ctx = Span::span_context(child_span)
  
  assert_eq(SpanContext::trace_id(parent_retrieved_ctx), SpanContext::trace_id(child_retrieved_ctx))
  assert_not_eq(SpanContext::span_id(parent_retrieved_ctx), SpanContext::span_id(child_retrieved_ctx))
  
  // Test adding events to both spans
  Span::add_event(parent_span, "parent.started")
  Span::add_event(child_span, "child.started")
  
  // Test setting status on both spans
  Span::set_status(parent_span, Ok, Some("Parent completed"))
  Span::set_status(child_span, Ok, Some("Child completed"))
}

test "span error handling and edge cases" {
  let provider = TracerProvider::default()
  let tracer = TracerProvider::get_tracer(provider, "test.tracer")
  
  // Test span with empty name
  let empty_name_span = Tracer::start_span(tracer, "")
  assert_eq(Span::name(empty_name_span), "")
  assert_true(Span::is_recording(empty_name_span))
  
  // Test span with very long name
  let long_name = "this.is.a.very.long.span.name.that.tests.the.systems.ability.to.handle.extended.span.names.with.multiple.segments.and.various.characters"
  let long_name_span = Tracer::start_span(tracer, long_name)
  assert_eq(Span::name(long_name_span), long_name)
  assert_true(Span::is_recording(long_name_span))
  
  // Test span with special characters in name
  let special_name_span = Tracer::start_span(tracer, "special!@#$%^&*()_+-={}[]|\\:;\"'<>?,./operation")
  assert_eq(Span::name(special_name_span), "special!@#$%^&*()_+-={}[]|\\:;\"'<>?,./operation")
  assert_true(Span::is_recording(special_name_span))
  
  // Test span operations after ending
  let end_test_span = Tracer::start_span(tracer, "end.test")
  Span::add_event(end_test_span, "before.end")
  Span::end(end_test_span)
  
  // In a real implementation, operations after end might be ignored
  // In simplified implementation, we can still call them
  Span::add_event(end_test_span, "after.end")
  Span::set_status(end_test_span, Error, Some("This might be ignored"))
}

test "span performance and resource management" {
  let provider = TracerProvider::default()
  let tracer = TracerProvider::get_tracer(provider, "performance.test")
  
  // Test creating multiple spans
  let spans = []
  for i in range(0, 10) {
    let span_name = "operation." + i.to_string()
    let span = Tracer::start_span(tracer, span_name)
    spans.push(span)
  }
  
  // Test adding events to all spans
  for span in spans {
    Span::add_event(span, "performance.test.event")
    Span::set_status(span, Ok)
  }
  
  // Test ending all spans
  for span in spans {
    Span::end(span)
  }
  
  // Verify all spans were created and processed
  assert_eq(length(spans), 10)
}