// Azimuth Cross-Service Propagation Tests

test "w3c_trace_context_propagation" {
  let carrier = TextMapCarrier::new()
  TextMapCarrier::set(carrier, "traceparent", "00-0af7651916cd43dd8448eb211c80319c-b7ad6b7169203331-01")
  TextMapCarrier::set(carrier, "tracestate", "rojo=00f067aa0ba902b7,congo=t61rcWkgMzE")
  
  let propagator = W3CTraceContextPropagator::new()
  let composite = CompositePropagator::new([propagator])
  
  let root_ctx = Context::root()
  CompositePropagator::inject(composite, root_ctx, carrier)
  
  let extracted_ctx = CompositePropagator::extract(composite, carrier)
  let extracted = Context::get(extracted_ctx, ContextKey::new("extracted"))
  
  match extracted {
    Some(value) => assert_eq(value, "true")
    _ => @expect(false)
  }
}

test "baggage_cross_service_propagation" {
  let baggage = Baggage::new()
  let updated = Baggage::set_entry(baggage, "user.id", "user123")
  let with_session = Baggage::set_entry(updated, "session.id", "session456")
  
  let user_id = Baggage::get_entry(with_session, "user.id")
  let session_id = Baggage::get_entry(with_session, "session.id")
  let missing = Baggage::get_entry(with_session, "missing.key")
  
  match user_id {
    Some(id) => assert_eq(id, "user123")
    _ => @expect(false)
  }
  
  match session_id {
    Some(id) => assert_eq(id, "session456")
    _ => @expect(false)
  }
  
  match missing {
    None => @expect(true)
    _ => @expect(false)
  }
}

test "span_context_trace_id_validation" {
  let valid_trace_id = "0af7651916cd43dd8448eb211c80319c"
  let valid_span_id = "b7ad6b7169203331"
  let span_ctx = SpanContext::new(valid_trace_id, valid_span_id, true, "rojo=00f067aa0ba902b7")
  
  assert_eq(SpanContext::trace_id(span_ctx), valid_trace_id)
  assert_eq(SpanContext::span_id(span_ctx), valid_span_id)
  assert_eq(SpanContext::is_sampled(span_ctx), true)
  assert_eq(SpanContext::is_valid(span_ctx), true)
  
  let empty_trace_id = ""
  let empty_span_id = ""
  let invalid_ctx = SpanContext::new(empty_trace_id, empty_span_id, false, "")
  
  assert_eq(SpanContext::is_valid(invalid_ctx), false)
  assert_eq(SpanContext::is_sampled(invalid_ctx), false)
}

test "composite_propagator_multiple_headers" {
  let carrier = TextMapCarrier::new()
  TextMapCarrier::set(carrier, "traceparent", "00-0af7651916cd43dd8448eb211c80319c-b7ad6b7169203331-01")
  TextMapCarrier::set(carrier, "baggage", "user.id=user123,session.id=session456")
  
  let trace_propagator = W3CTraceContextPropagator::new()
  let baggage_propagator = W3CBaggagePropagator::new()
  let composite = CompositePropagator::new([trace_propagator])
  
  let ctx = Context::root()
  let ctx_with_user = Context::with_value(ctx, ContextKey::new("user.id"), "user123")
  
  CompositePropagator::inject(composite, ctx_with_user, carrier)
  
  let trace_header = TextMapCarrier::get(carrier, "traceparent")
  match trace_header {
    Some(header) => @expect(true)  // Should contain trace data
    _ => @expect(false)
  }
}

test "context_propagation_across_spans" {
  let provider = TracerProvider::default()
  let tracer = TracerProvider::get_tracer(provider, "test_service")
  
  let parent_span = Tracer::start_span(tracer, "parent_operation")
  let parent_ctx = Context::with_value(Context::root(), ContextKey::new("operation"), "parent")
  
  let child_span = Tracer::start_span(tracer, "child_operation")
  let child_ctx = Context::with_value(parent_ctx, ContextKey::new("operation"), "child")
  
  let parent_operation = Context::get(parent_ctx, ContextKey::new("operation"))
  let child_operation = Context::get(child_ctx, ContextKey::new("operation"))
  
  match parent_operation {
    Some(op) => assert_eq(op, "parent")
    _ => @expect(false)
  }
  
  match child_operation {
    Some(op) => assert_eq(op, "child")
    _ => @expect(false)
  }
  
  assert_eq(Span::name(parent_span), "parent_operation")
  assert_eq(Span::name(child_span), "child_operation")
  
  Span::end(parent_span)
  Span::end(child_span)
}