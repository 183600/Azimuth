// Azimuth Telemetry System - Distributed Tracing Tests
// This file contains test cases for distributed tracing functionality

// Test 1: Trace Context Propagation
test "trace context propagation" {
  let provider = TracerProvider::new()
  let tracer = TracerProvider::get_tracer(provider, "distributed_tracer")
  
  // Create root span
  let root_span = Tracer::start_span(tracer, "root_operation")
  let root_context = Span::span_context(root_span)
  
  // Test trace context extraction
  let trace_id = SpanContext::trace_id(root_context)
  let span_id = SpanContext::span_id(root_context)
  
  assert_true(trace_id.length() > 0)
  assert_true(span_id.length() > 0)
  
  // Test context propagation to child span
  let child_span = Tracer::start_span_with_context(tracer, "child_operation", root_context)
  let child_context = Span::span_context(child_span)
  
  // Verify trace ID is propagated
  assert_eq(SpanContext::trace_id(child_context), trace_id)
  
  // Verify span ID is different
  assert_true(SpanContext::span_id(child_context) != span_id)
  
  // Test parent-child relationship
  let parent_span_id = SpanContext::parent_span_id(child_context)
  assert_eq(parent_span_id, span_id)
  
  Span::end(child_span)
  Span::end(root_span)
}

// Test 2: Cross-Service Trace Propagation
test "cross-service trace propagation" {
  let provider = TracerProvider::new()
  let tracer = TracerProvider::get_tracer(provider, "cross_service_tracer")
  
  // Simulate service A initiating a trace
  let service_a_span = Tracer::start_span(tracer, "service_a_operation")
  let service_a_context = Span::span_context(service_a_span)
  
  // Extract context for propagation
  let trace_headers = TraceContext::extract_headers(service_a_context)
  assert_true(trace_headers.contains("traceparent"))
  assert_true(trace_headers.contains("tracestate"))
  
  // Simulate service B receiving the trace
  let service_b_context = TraceContext::from_headers(trace_headers)
  let service_b_span = Tracer::start_span_with_context(tracer, "service_b_operation", service_b_context)
  
  // Verify trace continuity
  assert_eq(
    SpanContext::trace_id(service_a_context),
    SpanContext::trace_id(service_b_context)
  )
  
  // Test baggage propagation
  Baggage::set(service_a_context, "user.id", "12345")
  Baggage::set(service_a_context, "request.id", "req-67890")
  
  let propagated_baggage = Baggage::get_all(service_b_context)
  assert_true(propagated_baggage.length() >= 2)
  
  Span::end(service_b_span)
  Span::end(service_a_span)
}

// Test 3: Distributed Trace Sampling
test "distributed trace sampling" {
  let provider = TracerProvider::new()
  let tracer = TracerProvider::get_tracer(provider, "sampling_tracer")
  
  // Test always-on sampler
  let always_on_sampler = Sampler::always_on()
  let sampled_context = always_on_sampler.should_sample(
    "trace_id_123",
    "span_id_456",
    "operation_name",
    SpanKind::Server,
    Attributes::new()
  )
  assert_true(sampled_context.is_sampled)
  
  // Test always-off sampler
  let always_off_sampler = Sampler::always_off()
  let not_sampled_context = always_off_sampler.should_sample(
    "trace_id_789",
    "span_id_012",
    "operation_name",
    SpanKind::Server,
    Attributes::new()
  )
  assert_false(not_sampled_context.is_sampled)
  
  // Test trace ID ratio sampler
  let ratio_sampler = Sampler::trace_id_ratio(0.5) // 50% sampling
  let ratio_context1 = ratio_sampler.should_sample(
    "trace_id_abc", // This will be sampled based on trace ID
    "span_id_def",
    "operation_name",
    SpanKind::Server,
    Attributes::new()
  )
  
  // Test parent-based sampler
  let parent_based_sampler = Sampler::parent_based(always_on_sampler)
  let parent_sampled = SpanContext::new("trace_id", "parent_span", true, "")
  let child_sampled = parent_based_sampler.should_sample(
    "trace_id",
    "child_span",
    "operation_name",
    SpanKind::Client,
    Attributes::new(),
    Some(parent_sampled)
  )
  assert_true(child_sampled.is_sampled)
  
  let parent_not_sampled = SpanContext::new("trace_id_2", "parent_span_2", false, "")
  let child_not_sampled = parent_based_sampler.should_sample(
    "trace_id_2",
    "child_span_2",
    "operation_name",
    SpanKind::Client,
    Attributes::new(),
    Some(parent_not_sampled)
  )
  assert_false(child_not_sampled.is_sampled)
}

// Test 4: Distributed Trace Linking
test "distributed trace linking" {
  let provider = TracerProvider::new()
  let tracer = TracerProvider::get_tracer(provider, "linking_tracer")
  
  // Create related spans
  let related_context1 = SpanContext::new("trace_id_1", "span_id_1", true, "")
  let related_context2 = SpanContext::new("trace_id_2", "span_id_2", true, "")
  
  // Create links
  let links = [
    SpanLink::new(related_context1, Some("Related operation 1")),
    SpanLink::new(related_context2, Some("Related operation 2"))
  ]
  
  // Create span with links
  let linked_span = Tracer::start_span_with_links(tracer, "linked_operation", links)
  
  // Verify links are properly attached
  let span_links = Span::links(linked_span)
  assert_eq(span_links.length(), 2)
  
  let first_link = span_links[0]
  assert_eq(SpanLink::context(first_link), related_context1)
  match SpanLink::attributes(first_link) {
    Some(attrs) => {
      let link_name = Attributes::get(attrs, "link.name")
      match link_name {
        Some(StringValue(name)) => assert_eq(name, "Related operation 1")
        _ => assert_true(false)
      }
    }
    None => assert_true(false)
  }
  
  Span::end(linked_span)
}

// Test 5: Distributed Trace Events
test "distributed trace events" {
  let provider = TracerProvider::new()
  let tracer = TracerProvider::get_tracer(provider, "events_tracer")
  
  let span = Tracer::start_span(tracer, "operation_with_events")
  
  // Add events with attributes
  let event1_attrs = Attributes::new()
  Attributes::set(event1_attrs, "event.type", StringValue("error"))
  Attributes::set(event1_attrs, "error.code", IntValue(500))
  
  Span::add_event_with_time(span, "operation_failed", 1234567890L, Some(event1_attrs))
  
  let event2_attrs = Attributes::new()
  Attributes::set(event2_attrs, "event.type", StringValue("retry"))
  Attributes::set(event2_attrs, "retry.count", IntValue(1))
  
  Span::add_event_with_time(span, "retry_attempt", 1234567891L, Some(event2_attrs))
  
  // Verify events are recorded
  let events = Span::events(span)
  assert_eq(events.length(), 2)
  
  let first_event = events[0]
  assert_eq(SpanEvent::name(first_event), "operation_failed")
  assert_eq(SpanEvent::timestamp(first_event), 1234567890L)
  
  let first_event_attrs = SpanEvent::attributes(first_event)
  match first_event_attrs {
    Some(attrs) => {
      let event_type = Attributes::get(attrs, "event.type")
      match event_type {
        Some(StringValue(event_type_val)) => assert_eq(event_type_val, "error")
        _ => assert_true(false)
      }
      
      let error_code = Attributes::get(attrs, "error.code")
      match error_code {
        Some(IntValue(code)) => assert_eq(code, 500)
        _ => assert_true(false)
      }
    }
    None => assert_true(false)
  }
  
  Span::end(span)
}

// Test 6: Distributed Trace Status
test "distributed trace status" {
  let provider = TracerProvider::new()
  let tracer = TracerProvider::get_tracer(provider, "status_tracer")
  
  let span = Tracer::start_span(tracer, "operation_with_status")
  
  // Test initial status
  assert_eq(Span::status(span), Unset)
  
  // Test setting OK status
  Span::set_status(span, Ok, Some("Operation completed successfully"))
  assert_eq(Span::status(span), Ok)
  match Span::status_description(span) {
    Some(desc) => assert_eq(desc, "Operation completed successfully")
    None => assert_true(false)
  }
  
  // Test setting error status
  Span::set_status(span, Error, Some("Operation failed with timeout"))
  assert_eq(Span::status(span), Error)
  match Span::status_description(span) {
    Some(desc) => assert_eq(desc, "Operation failed with timeout")
    None => assert_true(false)
  }
  
  Span::end(span)
}

// Test 7: Distributed Trace Span Builder
test "distributed trace span builder" {
  let provider = TracerProvider::new()
  let tracer = TracerProvider::get_tracer(provider, "builder_tracer")
  
  // Test span builder pattern
  let span = SpanBuilder::new(tracer, "builder_operation")
    .with_kind(SpanKind::Server)
    .with_attribute("service.name", StringValue("test_service"))
    .with_attribute("service.version", StringValue("1.0.0"))
    .with_start_time(1234567890L)
    .start()
  
  // Verify span properties
  assert_eq(Span::name(span), "builder_operation")
  match Span::kind(span) {
    Server => assert_true(true)
    _ => assert_true(false)
  }
  
  let attributes = Span::attributes(span)
  let service_name = Attributes::get(attributes, "service.name")
  match service_name {
    Some(StringValue(name)) => assert_eq(name, "test_service")
    _ => assert_true(false)
  }
  
  let service_version = Attributes::get(attributes, "service.version")
  match service_version {
    Some(StringValue(version)) => assert_eq(version, "1.0.0")
    _ => assert_true(false)
  }
  
  Span::end(span)
}

// Test 8: Distributed Trace Context Extraction
test "distributed trace context extraction" {
  let provider = TracerProvider::new()
  let tracer = TracerProvider::get_tracer(provider, "extraction_tracer")
  
  // Create original span
  let original_span = Tracer::start_span(tracer, "original_operation")
  let original_context = Span::span_context(original_span)
  
  // Extract context to headers
  let headers = TraceContext::extract_headers(original_context)
  
  // Test extraction from different header formats
  let w3c_context = TraceContext::extract_from_w3c_headers(headers)
  assert_true(SpanContext::is_valid(w3c_context))
  
  let b3_context = TraceContext::extract_from_b3_headers(headers)
  assert_true(SpanContext::is_valid(b3_context))
  
  // Test context injection
  let injected_headers = TraceContext::inject_headers(w3c_context)
  assert_true(injected_headers.contains("traceparent"))
  
  // Test baggage extraction and injection
  Baggage::set(original_context, "user.id", "12345")
  Baggage::set(original_context, "session.id", "session-abc")
  
  let baggage_headers = Baggage::extract_headers(original_context)
  assert_true(baggage_headers.contains("baggage"))
  
  let extracted_baggage = Baggage::from_headers(baggage_headers)
  let user_id = Baggage::get(extracted_baggage, "user.id")
  match user_id {
    Some(value) => assert_eq(value, "12345")
    None => assert_true(false)
  }
  
  Span::end(original_span)
}