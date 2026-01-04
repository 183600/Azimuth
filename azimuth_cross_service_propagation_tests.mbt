// Azimuth Telemetry System - Cross-Service Propagation Tests
// This file contains comprehensive test cases for cross-service propagation functionality

// Test 1: Trace Context Propagation
test "trace context propagation" {
  // Create initial trace context
  let trace_id = "0af7651916cd43dd8448eb211c80319c"
  let span_id = "b7ad6b7169203331"
  let parent_span = SpanContext::new(trace_id, span_id, true, "")
  
  // Create propagator
  let propagator = TraceContextPropagator::new()
  
  // Inject context into carrier
  let carrier = TextMapCarrier::new()
  propagator.inject(parent_span, carrier)
  
  // Verify injected headers
  let trace_header = TextMapCarrier::get(carrier, "traceparent")
  match trace_header {
    Some(header) => {
      assert_true(header.contains("00-" + trace_id))
      assert_true(header.contains("-" + span_id))
      assert_true(header.contains("-01"))
    }
    None => assert_true(false)
  }
  
  let state_header = TextMapCarrier::get(carrier, "tracestate")
  match state_header {
    Some(_) => assert_true(true)
    None => assert_true(true) // Optional header
  }
  
  // Extract context from carrier
  let extracted_span = propagator.extract(carrier)
  
  // Verify extracted context matches original
  assert_eq(SpanContext::trace_id(extracted_span), trace_id)
  assert_eq(SpanContext::span_id(extracted_span), span_id)
  assert_true(SpanContext::is_sampled(extracted_span))
}

// Test 2: Baggage Propagation
test "baggage propagation" {
  // Create baggage with entries
  let baggage = Baggage::new()
  let updated_baggage = Baggage::set_entry(baggage, "user.id", "12345")
  let updated_baggage = Baggage::set_entry(updated_baggage, "session.id", "abcdef")
  let updated_baggage = Baggage::set_entry(updated_baggage, "request.id", "req-123")
  
  // Create propagator
  let propagator = BaggagePropagator::new()
  
  // Inject baggage into carrier
  let carrier = TextMapCarrier::new()
  propagator.inject(updated_baggage, carrier)
  
  // Verify injected baggage header
  let baggage_header = TextMapCarrier::get(carrier, "baggage")
  match baggage_header {
    Some(header) => {
      assert_true(header.contains("user.id=12345"))
      assert_true(header.contains("session.id=abcdef"))
      assert_true(header.contains("request.id=req-123"))
    }
    None => assert_true(false)
  }
  
  // Extract baggage from carrier
  let extracted_baggage = propagator.extract(carrier)
  
  // Verify extracted baggage entries
  let user_id = Baggage::get_entry(extracted_baggage, "user.id")
  match user_id {
    Some(value) => assert_eq(value, "12345")
    None => assert_true(false)
  }
  
  let session_id = Baggage::get_entry(extracted_baggage, "session.id")
  match session_id {
    Some(value) => assert_eq(value, "abcdef")
    None => assert_true(false)
  }
  
  let request_id = Baggage::get_entry(extracted_baggage, "request.id")
  match request_id {
    Some(value) => assert_eq(value, "req-123")
    None => assert_true(false)
  }
}

// Test 3: Correlation Context Propagation
test "correlation context propagation" {
  // Create correlation context
  let correlation_context = CorrelationContext::new()
  let updated_context = CorrelationContext::set_value(correlation_context, "correlation.id", "corr-12345")
  let updated_context = CorrelationContext::set_value(updated_context, "business.transaction", "order-processing")
  let updated_context = CorrelationContext::set_value(updated_context, "tenant.id", "tenant-001")
  
  // Create propagator
  let propagator = CorrelationContextPropagator::new()
  
  // Inject context into carrier
  let carrier = TextMapCarrier::new()
  propagator.inject(updated_context, carrier)
  
  // Verify injected correlation headers
  let correlation_header = TextMapCarrier::get(carrier, "correlation-context")
  match correlation_header {
    Some(header) => {
      assert_true(header.contains("correlation.id=corr-12345"))
      assert_true(header.contains("business.transaction=order-processing"))
      assert_true(header.contains("tenant.id=tenant-001"))
    }
    None => assert_true(false)
  }
  
  // Extract context from carrier
  let extracted_context = propagator.extract(carrier)
  
  // Verify extracted context values
  let correlation_id = CorrelationContext::get_value(extracted_context, "correlation.id")
  match correlation_id {
    Some(value) => assert_eq(value, "corr-12345")
    None => assert_true(false)
  }
  
  let business_transaction = CorrelationContext::get_value(extracted_context, "business.transaction")
  match business_transaction {
    Some(value) => assert_eq(value, "order-processing")
    None => assert_true(false)
  }
  
  let tenant_id = CorrelationContext::get_value(extracted_context, "tenant.id")
  match tenant_id {
    Some(value) => assert_eq(value, "tenant-001")
    None => assert_true(false)
  }
}

// Test 4: Composite Propagation
test "composite propagation" {
  // Create trace context
  let trace_id = "0af7651916cd43dd8448eb211c80319c"
  let span_id = "b7ad6b7169203331"
  let span_context = SpanContext::new(trace_id, span_id, true, "")
  
  // Create baggage
  let baggage = Baggage::new()
  let baggage = Baggage::set_entry(baggage, "user.id", "12345")
  
  // Create correlation context
  let correlation_context = CorrelationContext::new()
  let correlation_context = CorrelationContext::set_value(correlation_context, "correlation.id", "corr-12345")
  
  // Create composite propagator
  let composite_propagator = CompositePropagator::new([
    TraceContextPropagator::new(),
    BaggagePropagator::new(),
    CorrelationContextPropagator::new()
  ])
  
  // Create context with all components
  let context = Context::with_value(Context::root(), ContextKey::new("span_context"), span_context)
  let context = Context::with_value(context, ContextKey::new("baggage"), baggage)
  let context = Context::with_value(context, ContextKey::new("correlation_context"), correlation_context)
  
  // Inject all contexts into carrier
  let carrier = TextMapCarrier::new()
  composite_propagator.inject(context, carrier)
  
  // Verify all headers were injected
  assert_true(TextMapCarrier::contains_key(carrier, "traceparent"))
  assert_true(TextMapCarrier::contains_key(carrier, "baggage"))
  assert_true(TextMapCarrier::contains_key(carrier, "correlation-context"))
  
  // Extract all contexts from carrier
  let extracted_context = composite_propagator.extract(carrier)
  
  // Verify trace context was extracted
  let extracted_span = Context::get_value(extracted_context, ContextKey::new("span_context"))
  match extracted_span {
    Some(span_ctx) => {
      assert_eq(SpanContext::trace_id(span_ctx), trace_id)
      assert_eq(SpanContext::span_id(span_ctx), span_id)
    }
    None => assert_true(false)
  }
  
  // Verify baggage was extracted
  let extracted_baggage = Context::get_value(extracted_context, ContextKey::new("baggage"))
  match extracted_baggage {
    Some(bag) => {
      let user_id = Baggage::get_entry(bag, "user.id")
      match user_id {
        Some(value) => assert_eq(value, "12345")
        None => assert_true(false)
      }
    }
    None => assert_true(false)
  }
  
  // Verify correlation context was extracted
  let extracted_correlation = Context::get_value(extracted_context, ContextKey::new("correlation_context"))
  match extracted_correlation {
    Some(correlation) => {
      let correlation_id = CorrelationContext::get_value(correlation, "correlation.id")
      match correlation_id {
        Some(value) => assert_eq(value, "corr-12345")
        None => assert_true(false)
      }
    }
    None => assert_true(false)
  }
}

// Test 5: HTTP Header Propagation
test "http header propagation" {
  // Create trace context
  let trace_id = "0af7651916cd43dd8448eb211c80319c"
  let span_id = "b7ad6b7169203331"
  let span_context = SpanContext::new(trace_id, span_id, true, "")
  
  // Create baggage
  let baggage = Baggage::new()
  let baggage = Baggage::set_entry(baggage, "user.id", "12345")
  
  // Create HTTP headers carrier
  let http_headers = HttpHeaders::new()
  
  // Create propagator
  let propagator = CompositePropagator::new([
    TraceContextPropagator::new(),
    BaggagePropagator::new()
  ])
  
  // Create context
  let context = Context::with_value(Context::root(), ContextKey::new("span_context"), span_context)
  let context = Context::with_value(context, ContextKey::new("baggage"), baggage)
  
  // Inject into HTTP headers
  propagator.inject(context, http_headers)
  
  // Verify HTTP headers
  assert_true(HttpHeaders::contains(http_headers, "traceparent"))
  assert_true(HttpHeaders::contains(http_headers, "baggage"))
  
  let trace_header = HttpHeaders::get(http_headers, "traceparent")
  match trace_header {
    Some(header) => {
      assert_true(header.contains("00-" + trace_id))
      assert_true(header.contains("-" + span_id))
    }
    None => assert_true(false)
  }
  
  let baggage_header = HttpHeaders::get(http_headers, "baggage")
  match baggage_header {
    Some(header) => assert_true(header.contains("user.id=12345"))
    None => assert_true(false)
  }
  
  // Extract from HTTP headers
  let extracted_context = propagator.extract(http_headers)
  
  // Verify extraction
  let extracted_span = Context::get_value(extracted_context, ContextKey::new("span_context"))
  match extracted_span {
    Some(span_ctx) => {
      assert_eq(SpanContext::trace_id(span_ctx), trace_id)
      assert_eq(SpanContext::span_id(span_ctx), span_id)
    }
    None => assert_true(false)
  }
}

// Test 6: Message Queue Propagation
test "message queue propagation" {
  // Create trace context
  let trace_id = "0af7651916cd43dd8448eb211c80319c"
  let span_id = "b7ad6b7169203331"
  let span_context = SpanContext::new(trace_id, span_id, true, "")
  
  // Create baggage
  let baggage = Baggage::new()
  let baggage = Baggage::set_entry(baggage, "user.id", "12345")
  
  // Create message properties carrier
  let message_properties = MessageProperties::new()
  
  // Create propagator
  let propagator = CompositePropagator::new([
    TraceContextPropagator::new(),
    BaggagePropagator::new()
  ])
  
  // Create context
  let context = Context::with_value(Context::root(), ContextKey::new("span_context"), span_context)
  let context = Context::with_value(context, ContextKey::new("baggage"), baggage)
  
  // Inject into message properties
  propagator.inject(context, message_properties)
  
  // Verify message properties
  assert_true(MessageProperties::contains_key(message_properties, "traceparent"))
  assert_true(MessageProperties::contains_key(message_properties, "baggage"))
  
  let trace_prop = MessageProperties::get(message_properties, "traceparent")
  match trace_prop {
    Some(prop) => {
      assert_true(prop.contains("00-" + trace_id))
      assert_true(prop.contains("-" + span_id))
    }
    None => assert_true(false)
  }
  
  let baggage_prop = MessageProperties::get(message_properties, "baggage")
  match baggage_prop {
    Some(prop) => assert_true(prop.contains("user.id=12345"))
    None => assert_true(false)
  }
  
  // Extract from message properties
  let extracted_context = propagator.extract(message_properties)
  
  // Verify extraction
  let extracted_span = Context::get_value(extracted_context, ContextKey::new("span_context"))
  match extracted_span {
    Some(span_ctx) => {
      assert_eq(SpanContext::trace_id(span_ctx), trace_id)
      assert_eq(SpanContext::span_id(span_ctx), span_id)
    }
    None => assert_true(false)
  }
}

// Test 7: Cross-Service Trace Continuity
test "cross-service trace continuity" {
  // Service A: Create initial span
  let service_a_tracer = TracerProvider::get_tracer("service-a")
  let root_span = Tracer::start_span(service_a_tracer, "root-operation")
  let root_context = Span::span_context(root_span)
  
  // Propagate context to Service B
  let carrier = TextMapCarrier::new()
  let propagator = TraceContextPropagator::new()
  propagator.inject(root_context, carrier)
  
  // Service B: Extract context and create child span
  let extracted_context = propagator.extract(carrier)
  let service_b_tracer = TracerProvider::get_tracer("service-b")
  let child_span = Tracer::start_span_with_context(service_b_tracer, "child-operation", extracted_context)
  let child_context = Span::span_context(child_span)
  
  // Verify trace continuity
  assert_eq(SpanContext::trace_id(root_context), SpanContext::trace_id(child_context))
  assert_neq(SpanContext::span_id(root_context), SpanContext::span_id(child_context))
  
  // Propagate context to Service C
  let carrier2 = TextMapCarrier::new()
  propagator.inject(child_context, carrier2)
  
  // Service C: Extract context and create grandchild span
  let extracted_context2 = propagator.extract(carrier2)
  let service_c_tracer = TracerProvider::get_tracer("service-c")
  let grandchild_span = Tracer::start_span_with_context(service_c_tracer, "grandchild-operation", extracted_context2)
  let grandchild_context = Span::span_context(grandchild_span)
  
  // Verify trace continuity across all services
  assert_eq(SpanContext::trace_id(root_context), SpanContext::trace_id(grandchild_context))
  assert_neq(SpanContext::span_id(root_context), SpanContext::span_id(grandchild_context))
  assert_neq(SpanContext::span_id(child_context), SpanContext::span_id(grandchild_context))
  
  // End all spans
  Span::end(grandchild_span)
  Span::end(child_span)
  Span::end(root_span)
}

// Test 8: Propagation with Custom Headers
test "propagation with custom headers" {
  // Create custom propagator
  let custom_propagator = CustomPropagator::new("x-custom-trace", "x-custom-baggage")
  
  // Create trace context
  let trace_id = "0af7651916cd43dd8448eb211c80319c"
  let span_id = "b7ad6b7169203331"
  let span_context = SpanContext::new(trace_id, span_id, true, "")
  
  // Create baggage
  let baggage = Baggage::new()
  let baggage = Baggage::set_entry(baggage, "custom.key", "custom.value")
  
  // Create context
  let context = Context::with_value(Context::root(), ContextKey::new("span_context"), span_context)
  let context = Context::with_value(context, ContextKey::new("baggage"), baggage)
  
  // Inject with custom headers
  let carrier = TextMapCarrier::new()
  custom_propagator.inject(context, carrier)
  
  // Verify custom headers
  assert_true(TextMapCarrier::contains_key(carrier, "x-custom-trace"))
  assert_true(TextMapCarrier::contains_key(carrier, "x-custom-baggage"))
  
  let custom_trace = TextMapCarrier::get(carrier, "x-custom-trace")
  match custom_trace {
    Some(header) => {
      assert_true(header.contains(trace_id))
      assert_true(header.contains(span_id))
    }
    None => assert_true(false)
  }
  
  let custom_baggage = TextMapCarrier::get(carrier, "x-custom-baggage")
  match custom_baggage {
    Some(header) => assert_true(header.contains("custom.key=custom.value"))
    None => assert_true(false)
  }
  
  // Extract with custom headers
  let extracted_context = custom_propagator.extract(carrier)
  
  // Verify extraction
  let extracted_span = Context::get_value(extracted_context, ContextKey::new("span_context"))
  match extracted_span {
    Some(span_ctx) => {
      assert_eq(SpanContext::trace_id(span_ctx), trace_id)
      assert_eq(SpanContext::span_id(span_ctx), span_id)
    }
    None => assert_true(false)
  }
  
  let extracted_baggage = Context::get_value(extracted_context, ContextKey::new("baggage"))
  match extracted_baggage {
    Some(bag) => {
      let custom_value = Baggage::get_entry(bag, "custom.key")
      match custom_value {
        Some(value) => assert_eq(value, "custom.value")
        None => assert_true(false)
      }
    }
    None => assert_true(false)
  }
}