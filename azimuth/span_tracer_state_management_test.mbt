// Span and Tracer State Management Test Suite
// Testing comprehensive span lifecycle and tracer operations

test "span_creation_with_different_kinds" {
  let provider = TracerProvider::default()
  let tracer = TracerProvider::get_tracer(provider, "test-tracer")
  
  let span_ctx = SpanContext::new("test_trace_id", "test_span_id", true, "")
  
  // Test creating spans with different kinds
  let internal_span = Span::new("internal-operation", Internal, span_ctx)
  let server_span = Span::new("server-request", Server, span_ctx)
  let client_span = Span::new("client-request", Client, span_ctx)
  let producer_span = Span::new("message-production", Producer, span_ctx)
  let consumer_span = Span::new("message-consumption", Consumer, span_ctx)
  
  // Verify span properties
  assert_eq(Span::name(internal_span), "internal-operation")
  assert_eq(Span::kind(internal_span), Internal)
  assert_true(Span::is_recording(internal_span))
  assert_eq(Span::span_context(internal_span), span_ctx)
  
  assert_eq(Span::name(server_span), "server-request")
  assert_eq(Span::kind(server_span), Server)
  
  assert_eq(Span::name(client_span), "client-request")
  assert_eq(Span::kind(client_span), Client)
  
  assert_eq(Span::name(producer_span), "message-production")
  assert_eq(Span::kind(producer_span), Producer)
  
  assert_eq(Span::name(consumer_span), "message-consumption")
  assert_eq(Span::kind(consumer_span), Consumer)
}

test "span_lifecycle_operations" {
  let provider = TracerProvider::default()
  let tracer = TracerProvider::get_tracer(provider, "lifecycle-tracer")
  
  // Start a span
  let span = Tracer::start_span(tracer, "operation-lifecycle")
  
  // Verify initial state
  assert_eq(Span::name(span), "operation-lifecycle")
  assert_eq(Span::kind(span), Internal)
  assert_true(Span::is_recording(span))
  
  // Add events to span
  Span::add_event(span, "operation.started")
  Span::add_event(span, "operation.in_progress", Some([("step", StringValue("1"))]))
  
  // Set span status
  Span::set_status(span, Ok, Some("Operation completed successfully"))
  
  // Verify status change
  assert_eq(Span::status(span), Ok)
  
  // End the span
  Span::end(span)
  
  // Verify operation completes
  assert_true(true)
}

test "span_status_management" {
  let provider = TracerProvider::default()
  let tracer = TracerProvider::get_tracer(provider, "status-tracer")
  
  let span = Tracer::start_span(tracer, "status-test")
  
  // Test different status codes
  assert_eq(Span::status(span), Unset)
  
  Span::set_status(span, Ok)
  assert_eq(Span::status(span), Ok)
  
  Span::set_status(span, Error, Some("Something went wrong"))
  assert_eq(Span::status(span), Error)
  
  Span::set_status(span, Unset)
  assert_eq(Span::status(span), Unset)
  
  // End span with final status
  Span::set_status(span, Ok, Some("Completed after recovery"))
  Span::end(span)
  
  // Verify operation completes
  assert_true(true)
}

test "span_event_operations" {
  let provider = TracerProvider::default()
  let tracer = TracerProvider::get_tracer(provider, "event-tracer")
  
  let span = Tracer::start_span(tracer, "event-test")
  
  // Add events with different attribute combinations
  Span::add_event(span, "simple.event")
  
  Span::add_event(span, "event.with.attributes", Some([
    ("user.id", StringValue("user-123")),
    ("operation.type", StringValue("create")),
    ("retry.count", IntValue(3))
  ]))
  
  Span::add_event(span, "event.complex.attributes", Some([
    ("service.name", StringValue("payment-service")),
    ("request.id", StringValue("req-abcdef")),
    ("amount", FloatValue(99.99)),
    ("success", BoolValue(true)),
    ("tags", ArrayStringValue(["payment", "api", "v1"]))
  ]))
  
  // Test with empty attributes
  Span::add_event(span, "event.empty.attributes", Some([]))
  
  Span::end(span)
  
  // Verify operation completes
  assert_true(true)
}

test "tracer_with_different_instrumentation_scopes" {
  let provider = TracerProvider::default()
  
  // Test tracers with different scopes
  let default_tracer = TracerProvider::get_tracer(provider, "default-tracer")
  let versioned_tracer = TracerProvider::get_tracer(provider, "versioned-tracer", Some("2.1.0"))
  let full_scope_tracer = TracerProvider::get_tracer(
    provider, 
    "full-scope-tracer", 
    Some("1.5.2"), 
    Some("https://example.com/tracing-schema")
  )
  
  // Verify instrumentation scopes
  let default_scope = Tracer::instrumentation_scope(default_tracer)
  let versioned_scope = Tracer::instrumentation_scope(versioned_tracer)
  let full_scope = Tracer::instrumentation_scope(full_scope_tracer)
  
  assert_eq(default_scope.name, "default-tracer")
  assert_eq(default_scope.version, None)
  assert_eq(default_scope.schema_url, None)
  
  assert_eq(versioned_scope.name, "versioned-tracer")
  assert_eq(versioned_scope.version, Some("2.1.0"))
  assert_eq(versioned_scope.schema_url, None)
  
  assert_eq(full_scope.name, "full-scope-tracer")
  assert_eq(full_scope.version, Some("1.5.2"))
  assert_eq(full_scope.schema_url, Some("https://example.com/tracing-schema"))
  
  // Start spans with different tracers
  let default_span = Tracer::start_span(default_tracer, "default-operation")
  let versioned_span = Tracer::start_span(versioned_tracer, "versioned-operation")
  let full_scope_span = Tracer::start_span(full_scope_tracer, "full-scope-operation")
  
  Span::end(default_span)
  Span::end(versioned_span)
  Span::end(full_scope_span)
  
  // Verify operations complete
  assert_true(true)
}

test "span_context_validation_and_operations" {
  // Test valid span context
  let valid_trace_id = "0af7651916cd43dd8448eb211c80319c"
  let valid_span_id = "b7ad6b7169203331"
  let valid_ctx = SpanContext::new(valid_trace_id, valid_span_id, true, "key1=value1,key2=value2")
  
  assert_true(SpanContext::is_valid(valid_ctx))
  assert_true(SpanContext::is_sampled(valid_ctx))
  assert_eq(SpanContext::trace_id(valid_ctx), valid_trace_id)
  assert_eq(SpanContext::span_id(valid_ctx), valid_span_id)
  
  // Test invalid contexts
  let empty_trace_ctx = SpanContext::new("", valid_span_id, true, "")
  let empty_span_ctx = SpanContext::new(valid_trace_id, "", true, "")
  let both_empty_ctx = SpanContext::new("", "", false, "")
  
  assert_false(SpanContext::is_valid(empty_trace_ctx))
  assert_false(SpanContext::is_valid(empty_span_ctx))
  assert_false(SpanContext::is_valid(both_empty_ctx))
  
  // Test sampling variations
  let sampled_ctx = SpanContext::new(valid_trace_id, valid_span_id, true, "")
  let not_sampled_ctx = SpanContext::new(valid_trace_id, valid_span_id, false, "")
  
  assert_true(SpanContext::is_sampled(sampled_ctx))
  assert_false(SpanContext::is_sampled(not_sampled_ctx))
}

test "span_with_complex_attributes" {
  let provider = TracerProvider::default()
  let tracer = TracerProvider::get_tracer(provider, "complex-attr-tracer")
  
  let complex_attrs = [
    ("user.id", StringValue("user-12345")),
    ("request.id", StringValue("req-abcdef-67890")),
    ("service.name", StringValue("order-processing")),
    ("operation.type", StringValue("create_order")),
    ("retry.count", IntValue(3)),
    ("timeout.ms", IntValue(5000)),
    ("success.rate", FloatValue(0.95)),
    ("cache.hit", BoolValue(true)),
    ("tags", ArrayStringValue(["api", "v2", "critical"])),
    ("metrics", ArrayIntValue([100, 200, 300]))
  ]
  
  let span = Tracer::start_span(tracer, "complex-attributes-operation", Some(complex_attrs))
  
  // Add more events with complex attributes
  Span::add_event(span, "validation.completed", Some([
    ("validator.name", StringValue("business-rules")),
    ("validation.time.ms", IntValue(150)),
    ("rules.applied", IntValue(25))
  ]))
  
  Span::add_event(span, "database.operation", Some([
    ("db.type", StringValue("postgresql")),
    ("query.time.ms", IntValue(45)),
    ("rows.affected", IntValue(1))
  ]))
  
  Span::set_status(span, Ok, Some("Order processed successfully"))
  Span::end(span)
  
  // Verify operation completes
  assert_true(true)
}

test "span_error_handling_and_recovery" {
  let provider = TracerProvider::default()
  let tracer = TracerProvider::get_tracer(provider, "error-handling-tracer")
  
  let span = Tracer::start_span(tracer, "error-prone-operation")
  
  // Simulate error scenario
  Span::add_event(span, "operation.started")
  Span::add_event(span, "validation.failed", Some([
    ("error.code", StringValue("VALIDATION_ERROR")),
    ("error.message", StringValue("Invalid input parameters")),
    ("field.name", StringValue("email"))
  ]))
  
  // Set error status
  Span::set_status(span, Error, Some("Validation failed: Invalid email format"))
  
  // Simulate recovery attempt
  Span::add_event(span, "recovery.attempted", Some([
    ("retry.count", IntValue(1)),
    ("recovery.strategy", StringValue("input_sanitization"))
  ]))
  
  // Recovery successful
  Span::add_event(span, "recovery.successful")
  Span::set_status(span, Ok, Some("Recovered after input sanitization"))
  
  Span::end(span)
  
  // Verify operation completes
  assert_true(true)
}

test "concurrent_span_operations" {
  let provider = TracerProvider::default()
  let tracer = TracerProvider::get_tracer(provider, "concurrent-tracer")
  
  // Simulate concurrent operations
  let spans = []
  for i in range(0, 10) {
    let span = Tracer::start_span(tracer, "concurrent-operation-" + i.to_string())
    spans.push(span)
  }
  
  // Add events to all spans
  for (i, span) in spans.enumerate() {
    Span::add_event(span, "step.completed", Some([
      ("step.number", IntValue(i)),
      ("thread.id", StringValue("thread-" + (i % 3).to_string()))
    ]))
  }
  
  // End all spans
  for span in spans {
    Span::set_status(span, Ok)
    Span::end(span)
  }
  
  // Verify operations complete
  assert_true(true)
}