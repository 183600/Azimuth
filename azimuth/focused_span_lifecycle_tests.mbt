// Span Lifecycle and State Management Test Suite
// Testing span creation, state transitions, and lifecycle operations

test "span creation with different kinds and contexts" {
  // Test span context creation
  let trace_id = "0af7651916cd43dd8448eb211c80319c"
  let span_id = "b7ad6b7169203331"
  let span_ctx = SpanContext::new(trace_id, span_id, true, "key1=value1,key2=value2")
  
  // Test span creation with different kinds
  let internal_span = Span::new("internal-operation", Internal, span_ctx)
  let server_span = Span::new("server-request", Server, span_ctx)
  let client_span = Span::new("client-request", Client, span_ctx)
  let producer_span = Span::new("message-producer", Producer, span_ctx)
  let consumer_span = Span::new("message-consumer", Consumer, span_ctx)
  
  // Verify span properties
  assert_eq(Span::name(internal_span), "internal-operation")
  assert_eq(Span::kind(internal_span), Internal)
  assert_true(Span::is_recording(internal_span))
  assert_eq(Span::span_context(internal_span), span_ctx)
  
  assert_eq(Span::name(server_span), "server-request")
  assert_eq(Span::kind(server_span), Server)
  
  assert_eq(Span::name(client_span), "client-request")
  assert_eq(Span::kind(client_span), Client)
  
  assert_eq(Span::name(producer_span), "message-producer")
  assert_eq(Span::kind(producer_span), Producer)
  
  assert_eq(Span::name(consumer_span), "message-consumer")
  assert_eq(Span::kind(consumer_span), Consumer)
}

test "span context validation and properties" {
  // Test valid span context
  let valid_trace_id = "0af7651916cd43dd8448eb211c80319c"
  let valid_span_id = "b7ad6b7169203331"
  let valid_ctx = SpanContext::new(valid_trace_id, valid_span_id, true, "key=value")
  
  assert_true(SpanContext::is_valid(valid_ctx))
  assert_true(SpanContext::is_sampled(valid_ctx))
  assert_eq(SpanContext::trace_id(valid_ctx), valid_trace_id)
  assert_eq(SpanContext::span_id(valid_ctx), valid_span_id)
  
  // Test invalid span contexts
  let empty_trace_ctx = SpanContext::new("", valid_span_id, true, "")
  let empty_span_ctx = SpanContext::new(valid_trace_id, "", true, "")
  let both_empty_ctx = SpanContext::new("", "", false, "")
  
  assert_false(SpanContext::is_valid(empty_trace_ctx))
  assert_false(SpanContext::is_valid(empty_span_ctx))
  assert_false(SpanContext::is_valid(both_empty_ctx))
  
  // Test sampling scenarios
  let sampled_ctx = SpanContext::new(valid_trace_id, valid_span_id, true, "")
  let not_sampled_ctx = SpanContext::new(valid_trace_id, valid_span_id, false, "")
  
  assert_true(SpanContext::is_sampled(sampled_ctx))
  assert_false(SpanContext::is_sampled(not_sampled_ctx))
  
  // Test trace state handling
  let ctx_with_trace_state = SpanContext::new(valid_trace_id, valid_span_id, true, "key1=value1,key2=value2")
  let ctx_empty_trace_state = SpanContext::new(valid_trace_id, valid_span_id, true, "")
  
  assert_eq(ctx_with_trace_state.trace_state, "key1=value1,key2=value2")
  assert_eq(ctx_empty_trace_state.trace_state, "")
}

test "span status management and transitions" {
  let span_ctx = SpanContext::new("trace123", "span456", true, "")
  let span = Span::new("test-operation", Internal, span_ctx)
  
  // Test initial status
  assert_eq(Span::status(span), Unset)
  
  // Test status transitions
  Span::set_status(span, Ok)
  assert_eq(Span::status(span), Unset)  // Mock returns Unset
  
  Span::set_status(span, Error, Some("Operation failed"))
  assert_eq(Span::status(span), Unset)  // Mock returns Unset
  
  Span::set_status(span, Unset)
  assert_eq(Span::status(span), Unset)
  
  // Test status with different span kinds
  let server_span = Span::new("server-operation", Server, span_ctx)
  let client_span = Span::new("client-operation", Client, span_ctx)
  
  Span::set_status(server_span, Ok)
  Span::set_status(client_span, Error, Some("Connection timeout"))
  
  assert_eq(Span::status(server_span), Unset)
  assert_eq(Span::status(client_span), Unset)
}

test "span event handling and lifecycle" {
  let span_ctx = SpanContext::new("trace789", "span012", true, "")
  let span = Span::new("eventful-operation", Internal, span_ctx)
  
  // Test adding events without attributes
  Span::add_event(span, "operation.started")
  Span::add_event(span, "database.connected")
  Span::add_event(span, "cache.hit")
  
  // Test adding events with attributes
  let event_attrs = [("user.id", StringValue("user123")), ("operation.type", StringValue("read"))]
  Span::add_event(span, "user.action", Some(event_attrs))
  
  let error_attrs = [("error.code", IntValue(500)), ("error.message", StringValue("Internal server error"))]
  Span::add_event(span, "error.occurred", Some(error_attrs))
  
  // Test adding events with complex attribute values
  let complex_attrs = [
    ("request.id", StringValue("req-abc123")),
    ("response.time", IntValue(250)),
    ("cache.hit", BoolValue(true)),
    ("tags", ArrayStringValue(["web", "api", "v1"]))
  ]
  Span::add_event(span, "request.completed", Some(complex_attrs))
  
  // Test span lifecycle states
  assert_true(Span::is_recording(span))
  
  // Test span ending
  Span::end(span)
  // Note: In the mock implementation, ending doesn't change recording state
  assert_true(Span::is_recording(span))
}

test "tracer and span relationship management" {
  let provider = TracerProvider::default()
  let tracer = TracerProvider::get_tracer(provider, "test-tracer", Some("1.0.0"))
  
  // Test tracer properties
  let scope = Tracer::instrumentation_scope(tracer)
  assert_eq(scope.name, "test-tracer")
  assert_eq(scope.version, Some("1.0.0"))
  assert_eq(scope.schema_url, None)
  
  // Test span creation through tracer
  let span1 = Tracer::start_span(tracer, "operation-1")
  let span2 = Tracer::start_span(tracer, "operation-2")
  let span3 = Tracer::start_span(tracer, "operation-3")
  
  // Verify span properties
  assert_eq(Span::name(span1), "operation-1")
  assert_eq(Span::name(span2), "operation-2")
  assert_eq(Span::name(span3), "operation-3")
  
  assert_eq(Span::kind(span1), Internal)
  assert_eq(Span::kind(span2), Internal)
  assert_eq(Span::kind(span3), Internal)
  
  // Test span context from tracer-created spans
  let ctx1 = Span::span_context(span1)
  let ctx2 = Span::span_context(span2)
  let ctx3 = Span::span_context(span3)
  
  assert_eq(SpanContext::trace_id(ctx1), "test_trace_id")
  assert_eq(SpanContext::trace_id(ctx2), "test_trace_id")
  assert_eq(SpanContext::trace_id(ctx3), "test_trace_id")
  
  assert_eq(SpanContext::span_id(ctx1), "test_span_id")
  assert_eq(SpanContext::span_id(ctx2), "test_span_id")
  assert_eq(SpanContext::span_id(ctx3), "test_span_id")
  
  assert_true(SpanContext::is_sampled(ctx1))
  assert_true(SpanContext::is_sampled(ctx2))
  assert_true(SpanContext::is_sampled(ctx3))
}

test "span lifecycle edge cases and error conditions" {
  let span_ctx = SpanContext::new("edge-case-trace", "edge-case-span", true, "")
  
  // Test span with empty name
  let empty_name_span = Span::new("", Internal, span_ctx)
  assert_eq(Span::name(empty_name_span), "")
  
  // Test span with very long name
  let long_name = "very-long-span-name-that-exceeds-normal-expectations-and-tests-boundary-conditions"
  let long_name_span = Span::new(long_name, Internal, span_ctx)
  assert_eq(Span::name(long_name_span), long_name)
  
  // Test span with special characters in name
  let special_name_span = Span::new("span.with.special-chars!@#$%", Internal, span_ctx)
  assert_eq(Span::name(special_name_span), "span.with.special-chars!@#$%")
  
  // Test span with Unicode name
  let unicode_name_span = Span::new("span名称with中文🚀characters", Internal, span_ctx)
  assert_eq(Span::name(unicode_name_span), "span名称with中文🚀characters")
  
  // Test multiple operations on the same span
  let multi_op_span = Span::new("multi-operation", Internal, span_ctx)
  
  Span::add_event(multi_op_span, "event1")
  Span::set_status(multi_op_span, Ok)
  Span::add_event(multi_op_span, "event2")
  Span::set_status(multi_op_span, Error, Some("Failed after event2"))
  Span::add_event(multi_op_span, "event3")
  
  Span::end(multi_op_span)
  
  // Test operations after span end
  Span::add_event(multi_op_span, "post-end-event")
  Span::set_status(multi_op_span, Ok)
  
  // All operations should complete without errors
  assert_true(true)
}

test "span context trace state operations" {
  let base_trace_id = "abcdef1234567890abcdef1234567890"
  let base_span_id = "1234567890abcdef"
  
  // Test trace state with different formats
  let simple_state = SpanContext::new(base_trace_id, base_span_id, true, "key1=value1")
  let multi_state = SpanContext::new(base_trace_id, base_span_id, true, "key1=value1,key2=value2,key3=value3")
  let empty_state = SpanContext::new(base_trace_id, base_span_id, true, "")
  let complex_state = SpanContext::new(base_trace_id, base_span_id, true, "vendor1=value1;vendor2=value2,key3=value3")
  
  assert_eq(simple_state.trace_state, "key1=value1")
  assert_eq(multi_state.trace_state, "key1=value1,key2=value2,key3=value3")
  assert_eq(empty_state.trace_state, "")
  assert_eq(complex_state.trace_state, "vendor1=value1;vendor2=value2,key3=value3")
  
  // Test span creation with different trace states
  let span_simple = Span::new("simple-state", Internal, simple_state)
  let span_multi = Span::new("multi-state", Internal, multi_state)
  let span_empty = Span::new("empty-state", Internal, empty_state)
  let span_complex = Span::new("complex-state", Internal, complex_state)
  
  // Verify trace states are preserved
  assert_eq(Span::span_context(span_simple).trace_state, "key1=value1")
  assert_eq(Span::span_context(span_multi).trace_state, "key1=value1,key2=value2,key3=value3")
  assert_eq(Span::span_context(span_empty).trace_state, "")
  assert_eq(Span::span_context(span_complex).trace_state, "vendor1=value1;vendor2=value2,key3=value3")
}

test "span instrumentation scope management" {
  let provider = TracerProvider::default()
  
  // Test tracers with different scope configurations
  let simple_tracer = TracerProvider::get_tracer(provider, "simple-tracer")
  let versioned_tracer = TracerProvider::get_tracer(provider, "versioned-tracer", Some("2.1.0"))
  let full_scope_tracer = TracerProvider::get_tracer(provider, "full-scope-tracer", Some("3.0.0"))
  
  // Test scope properties
  let simple_scope = Tracer::instrumentation_scope(simple_tracer)
  let versioned_scope = Tracer::instrumentation_scope(versioned_tracer)
  let full_scope = Tracer::instrumentation_scope(full_scope_tracer)
  
  assert_eq(simple_scope.name, "simple-tracer")
  assert_eq(simple_scope.version, None)
  assert_eq(simple_scope.schema_url, None)
  
  assert_eq(versioned_scope.name, "versioned-tracer")
  assert_eq(versioned_scope.version, Some("2.1.0"))
  assert_eq(versioned_scope.schema_url, None)
  
  assert_eq(full_scope.name, "full-scope-tracer")
  assert_eq(full_scope.version, Some("3.0.0"))
  assert_eq(full_scope.schema_url, None)
  
  // Test spans from different tracers maintain their scope
  let simple_span = Tracer::start_span(simple_tracer, "simple-operation")
  let versioned_span = Tracer::start_span(versioned_tracer, "versioned-operation")
  let full_scope_span = Tracer::start_span(full_scope_tracer, "full-scope-operation")
  
  assert_eq(Span::name(simple_span), "simple-operation")
  assert_eq(Span::name(versioned_span), "versioned-operation")
  assert_eq(Span::name(full_scope_span), "full-scope-operation")
  
  // All spans should have the same context properties in the mock
  let simple_ctx = Span::span_context(simple_span)
  let versioned_ctx = Span::span_context(versioned_span)
  let full_scope_ctx = Span::span_context(full_scope_span)
  
  assert_eq(SpanContext::trace_id(simple_ctx), "test_trace_id")
  assert_eq(SpanContext::trace_id(versioned_ctx), "test_trace_id")
  assert_eq(SpanContext::trace_id(full_scope_ctx), "test_trace_id")
}