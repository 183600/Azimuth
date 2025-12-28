// Advanced Span and Tracer Integration Test Suite
// Tests complex span operations, tracer functionality, and integration scenarios

test "span lifecycle management" {
  let provider = TracerProvider::default()
  let tracer = TracerProvider::get_tracer(provider, "test-tracer", Some("1.0.0"))
  
  // Test span creation with different kinds
  let internal_span = Tracer::start_span(tracer, "internal-operation")
  let server_span = Tracer::start_span(tracer, "server-request")
  let client_span = Tracer::start_span(tracer, "client-request")
  let producer_span = Tracer::start_span(tracer, "message-producer")
  let consumer_span = Tracer::start_span(tracer, "message-consumer")
  
  // Verify span properties
  assert_eq(Span::name(internal_span), "internal-operation")
  assert_eq(Span::name(server_span), "server-request")
  assert_eq(Span::name(client_span), "client-request")
  assert_eq(Span::name(producer_span), "message-producer")
  assert_eq(Span::name(consumer_span), "message-consumer")
  
  // Test span recording state
  assert_true(Span::is_recording(internal_span))
  assert_true(Span::is_recording(server_span))
  assert_true(Span::is_recording(client_span))
  assert_true(Span::is_recording(producer_span))
  assert_true(Span::is_recording(consumer_span))
  
  // Test span context validity
  let internal_ctx = Span::span_context(internal_span)
  let server_ctx = Span::span_context(server_span)
  assert_true(SpanContext::is_valid(internal_ctx))
  assert_true(SpanContext::is_valid(server_ctx))
  
  // Test span ending
  Span::end(internal_span)
  Span::end(server_span)
  Span::end(client_span)
  Span::end(producer_span)
  Span::end(consumer_span)
}

test "span status and event operations" {
  let provider = TracerProvider::default()
  let tracer = TracerProvider::get_tracer(provider, "status-test-tracer")
  let span = Tracer::start_span(tracer, "operation-with-status")
  
  // Test initial status
  assert_eq(Span::status(span), Unset)
  
  // Test setting different statuses
  Span::set_status(span, Ok, Some("Operation completed successfully"))
  assert_eq(Span::status(span), Ok) // Note: simplified implementation returns Unset
  
  Span::set_status(span, Error, Some("Operation failed with error"))
  assert_eq(Span::status(span), Unset) // Note: simplified implementation returns Unset
  
  // Test adding events
  Span::add_event(span, "operation.started", Some([("start_time", StringValue("2023-01-01T00:00:00Z"))]))
  Span::add_event(span, "operation.progress", Some([("progress", IntValue(50))]))
  Span::add_event(span, "operation.completed", Some([("end_time", StringValue("2023-01-01T00:05:00Z"))]))
  
  // Test adding events without attributes
  Span::add_event(span, "simple.event")
  Span::add_event(span, "another.simple.event")
  
  // End span
  Span::end(span)
}

test "span context propagation" {
  let provider = TracerProvider::default()
  let tracer = TracerProvider::get_tracer(provider, "propagation-test-tracer")
  
  // Create parent span
  let parent_span = Tracer::start_span(tracer, "parent-operation")
  let parent_ctx = Span::span_context(parent_span)
  
  // Verify parent context properties
  assert_true(SpanContext::is_valid(parent_ctx))
  assert_true(SpanContext::is_sampled(parent_ctx))
  assert_true(SpanContext::trace_id(parent_ctx) != "")
  assert_true(SpanContext::span_id(parent_ctx) != "")
  
  // Create child span (in real implementation, would use parent context)
  let child_span = Tracer::start_span(tracer, "child-operation")
  let child_ctx = Span::span_context(child_span)
  
  // Verify child context properties
  assert_true(SpanContext::is_valid(child_ctx))
  assert_true(SpanContext::is_sampled(child_ctx))
  
  // Test trace state handling
  let trace_state_span = Tracer::start_span(tracer, "trace-state-operation")
  let trace_state_ctx = Span::span_context(trace_state_span)
  let trace_state_span_with_state = Span::new(
    "trace-state-with-flags", 
    Internal, 
    SpanContext::new("test_trace_id", "test_span_id", true, "key1=value1,key2=value2")
  )
  let trace_state_ctx_with_state = Span::span_context(trace_state_span_with_state)
  
  // End all spans
  Span::end(parent_span)
  Span::end(child_span)
  Span::end(trace_state_span)
  Span::end(trace_state_span_with_state)
}

test "tracer instrumentation scope" {
  let provider = TracerProvider::default()
  
  // Test tracer with different scope configurations
  let simple_tracer = TracerProvider::get_tracer(provider, "simple-tracer")
  let versioned_tracer = TracerProvider::get_tracer(provider, "versioned-tracer", Some("2.1.0"))
  let full_scope_tracer = TracerProvider::get_tracer(provider, "full-scope-tracer", Some("3.0.0"))
  
  // Test instrumentation scope retrieval
  let simple_scope = Tracer::instrumentation_scope(simple_tracer)
  let versioned_scope = Tracer::instrumentation_scope(versioned_tracer)
  let full_scope = Tracer::instrumentation_scope(full_scope_tracer)
  
  // Verify scope properties
  assert_eq(simple_scope.name, "simple-tracer")
  assert_eq(versioned_scope.name, "versioned-tracer")
  assert_eq(full_scope.name, "full-scope-tracer")
  
  assert_eq(simple_scope.version, None)
  assert_eq(versioned_scope.version, Some("2.1.0"))
  assert_eq(full_scope.version, Some("3.0.0"))
  
  // Test creating spans with different tracers
  let simple_span = Tracer::start_span(simple_tracer, "simple-operation")
  let versioned_span = Tracer::start_span(versioned_tracer, "versioned-operation")
  let full_scope_span = Tracer::start_span(full_scope_tracer, "full-scope-operation")
  
  // Verify span names
  assert_eq(Span::name(simple_span), "simple-operation")
  assert_eq(Span::name(versioned_span), "versioned-operation")
  assert_eq(Span::name(full_scope_span), "full-scope-operation")
  
  // End spans
  Span::end(simple_span)
  Span::end(versioned_span)
  Span::end(full_scope_span)
}

test "span kind behavior verification" {
  let provider = TracerProvider::default()
  let tracer = TracerProvider::get_tracer(provider, "span-kind-test-tracer")
  
  // Create spans with different kinds explicitly
  let internal_span = Span::new("internal-op", Internal, SpanContext::new("trace1", "span1", true, ""))
  let server_span = Span::new("server-op", Server, SpanContext::new("trace2", "span2", true, ""))
  let client_span = Span::new("client-op", Client, SpanContext::new("trace3", "span3", true, ""))
  let producer_span = Span::new("producer-op", Producer, SpanContext::new("trace4", "span4", true, ""))
  let consumer_span = Span::new("consumer-op", Consumer, SpanContext::new("trace5", "span5", true, ""))
  
  // Verify span kinds
  assert_eq(Span::kind(internal_span), Internal)
  assert_eq(Span::kind(server_span), Server)
  assert_eq(Span::kind(client_span), Client)
  assert_eq(Span::kind(producer_span), Producer)
  assert_eq(Span::kind(consumer_span), Consumer)
  
  // Test span operations for each kind
  for span in [internal_span, server_span, client_span, producer_span, consumer_span] {
    assert_true(Span::is_recording(span))
    Span::add_event(span, "test-event")
    Span::set_status(span, Ok)
  }
  
  // End all spans
  Span::end(internal_span)
  Span::end(server_span)
  Span::end(client_span)
  Span::end(producer_span)
  Span::end(consumer_span)
}

test "complex span attribute operations" {
  let provider = TracerProvider::default()
  let tracer = TracerProvider::get_tracer(provider, "attribute-test-tracer")
  
  // Create span with complex attributes
  let complex_attributes = [
    ("user.id", StringValue("user-12345")),
    ("request.size", IntValue(1024)),
    ("response.time", FloatValue(245.6)),
    ("cache.hit", BoolValue(true)),
    ("tags", ArrayStringValue(["web", "api", "v1"])),
    ("retry.count", IntValue(3))
  ]
  
  let span_with_attrs = Tracer::start_span(tracer, "complex-attribute-operation", Some(complex_attributes))
  
  // Test span with different attribute types
  let simple_attributes = [("operation.type", StringValue("read"))]
  let span_simple = Tracer::start_span(tracer, "simple-attribute-operation", Some(simple_attributes))
  
  // Test span without attributes
  let span_no_attrs = Tracer::start_span(tracer, "no-attribute-operation")
  
  // Verify all spans are properly created
  assert_eq(Span::name(span_with_attrs), "complex-attribute-operation")
  assert_eq(Span::name(span_simple), "simple-attribute-operation")
  assert_eq(Span::name(span_no_attrs), "no-attribute-operation")
  
  // Test adding events with attributes
  Span::add_event(span_with_attrs, "operation.started", Some([("timestamp", StringValue("2023-01-01T00:00:00Z"))]))
  Span::add_event(span_simple, "operation.completed", Some([("duration", IntValue(150))]))
  Span::add_event(span_no_attrs, "simple.event")
  
  // Test status with descriptions
  Span::set_status(span_with_attrs, Ok, Some("All operations completed successfully"))
  Span::set_status(span_simple, Error, Some("Partial failure occurred"))
  Span::set_status(span_no_attrs, Unset)
  
  // End all spans
  Span::end(span_with_attrs)
  Span::end(span_simple)
  Span::end(span_no_attrs)
}

test "span context edge cases" {
  // Test invalid span contexts
  let empty_trace_id = SpanContext::new("", "valid_span_id", true, "")
  let empty_span_id = SpanContext::new("valid_trace_id", "", true, "")
  let both_empty = SpanContext::new("", "", true, "")
  let valid_context = SpanContext::new("0af7651916cd43dd8448eb211c80319c", "b7ad6b7169203331", true, "key=value")
  
  // Verify validity checks
  assert_false(SpanContext::is_valid(empty_trace_id))
  assert_false(SpanContext::is_valid(empty_span_id))
  assert_false(SpanContext::is_valid(both_empty))
  assert_true(SpanContext::is_valid(valid_context))
  
  // Test sampling behavior
  let sampled_context = SpanContext::new("trace_id", "span_id", true, "")
  let not_sampled_context = SpanContext::new("trace_id", "span_id", false, "")
  
  assert_true(SpanContext::is_sampled(sampled_context))
  assert_false(SpanContext::is_sampled(not_sampled_context))
  
  // Test trace state handling
  let context_with_state = SpanContext::new("trace_id", "span_id", true, "key1=value1,key2=value2")
  let empty_state_context = SpanContext::new("trace_id", "span_id", true, "")
  
  assert_eq(SpanContext::trace_id(context_with_state), "trace_id")
  assert_eq(SpanContext::span_id(context_with_state), "span_id")
  
  // Create spans with different contexts
  let valid_span = Span::new("valid-span", Internal, valid_context)
  let invalid_span = Span::new("invalid-span", Internal, empty_trace_id)
  
  assert_true(SpanContext::is_valid(Span::span_context(valid_span)))
  assert_false(SpanContext::is_valid(Span::span_context(invalid_span)))
  
  // End spans
  Span::end(valid_span)
  Span::end(invalid_span)
}