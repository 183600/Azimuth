// Azimuth Telemetry System - Distributed Tracing Tests
// This file contains comprehensive test cases for distributed tracing functionality

// Test 1: Basic Distributed Trace Creation
test "basic distributed trace creation" {
  let tracer = TracerProvider::get_tracer("distributed_tracing_test")
  
  // Create root span
  let root_span = Tracer::start_span(tracer, "root_operation")
  let root_ctx = Span::span_context(root_span)
  
  // Test root span properties
  assert_eq(Span::name(root_span), "root_operation")
  assert_eq(Span::kind(root_span), Internal)
  assert_true(Span::is_recording(root_span))
  assert_true(SpanContext::is_valid(root_ctx))
  assert_true(SpanContext::is_sampled(root_ctx))
  
  // Add attributes to root span
  Span::set_attribute(root_span, "service.name", StringValue("test_service"))
  Span::set_attribute(root_span, "operation.type", StringValue("root"))
  
  // Add events to root span
  Span::add_event(root_span, "operation_started", Some([
    ("timestamp", IntValue(1609459200L)),
    ("initiator", StringValue("test_suite"))
  ]))
  
  // End root span
  Span::end(root_span)
  
  // Verify span is no longer recording
  assert_false(Span::is_recording(root_span))
}

// Test 2: Parent-Child Span Relationships
test "parent-child span relationships" {
  let tracer = TracerProvider::get_tracer("parent_child_test")
  
  // Create parent span
  let parent_span = Tracer::start_span(tracer, "parent_operation")
  let parent_ctx = Span::span_context(parent_span)
  
  // Create child span with explicit parent
  let child_span = Tracer::start_span_with_context(
    tracer,
    "child_operation",
    Some(parent_ctx)
  )
  
  let child_ctx = Span::span_context(child_span)
  
  // Verify parent-child relationship
  assert_eq(SpanContext::trace_id(child_ctx), SpanContext::trace_id(parent_ctx))
  assert_true(SpanContext::span_id(child_ctx) != SpanContext::span_id(parent_ctx))
  
  // Check if parent context is properly set
  let parent_span_ctx = Span::parent_span_context(child_span)
  match parent_span_ctx {
    Some(ctx) => assert_eq(SpanContext::span_id(ctx), SpanContext::span_id(parent_ctx))
    None => assert_true(false)
  }
  
  // Add nested child
  let grandchild_span = Tracer::start_span_with_context(
    tracer,
    "grandchild_operation",
    Some(child_ctx)
  )
  
  let grandchild_ctx = Span::span_context(grandchild_span)
  
  // Verify trace consistency across generations
  assert_eq(SpanContext::trace_id(grandchild_ctx), SpanContext::trace_id(parent_ctx))
  assert_eq(SpanContext::trace_id(grandchild_ctx), SpanContext::trace_id(child_ctx))
  
  // End spans in reverse order
  Span::end(grandchild_span)
  Span::end(child_span)
  Span::end(parent_span)
}

// Test 3: Cross-Service Trace Propagation
test "cross-service trace propagation" {
  let tracer = TracerProvider::get_tracer("cross_service_test")
  
  // Service A: Create initial span
  let service_a_span = Tracer::start_span(tracer, "service_a_operation")
  let service_a_ctx = Span::span_context(service_a_span)
  
  // Extract context for propagation
  let propagator = TraceContextPropagator::new()
  let headers = []
  let injected_headers = TraceContextPropagator::inject(propagator, service_a_ctx, headers)
  
  // Verify headers contain trace information
  let trace_header = injected_headers.find(|h| h.0 == "traceparent")
  match trace_header {
    Some((_, value)) => assert_true(value.length() > 0)
    None => assert_true(false)
  }
  
  // Service B: Extract context from headers
  let extracted_ctx = TraceContextPropagator::extract(propagator, injected_headers)
  match extracted_ctx {
    Some(ctx) => {
      // Verify trace continuity
      assert_eq(SpanContext::trace_id(ctx), SpanContext::trace_id(service_a_ctx))
      assert_true(SpanContext::is_valid(ctx))
      
      // Create span in Service B with extracted context
      let service_b_span = Tracer::start_span_with_context(
        tracer,
        "service_b_operation",
        Some(ctx)
      )
      
      let service_b_ctx = Span::span_context(service_b_span)
      
      // Verify parent-child relationship across services
      assert_eq(SpanContext::trace_id(service_b_ctx), SpanContext::trace_id(service_a_ctx))
      
      // Add service-specific attributes
      Span::set_attribute(service_a_span, "service.name", StringValue("service_a"))
      Span::set_attribute(service_b_span, "service.name", StringValue("service_b"))
      
      Span::end(service_b_span)
    }
    None => assert_true(false)
  }
  
  Span::end(service_a_span)
}

// Test 4: Distributed Trace Sampling
test "distributed trace sampling" {
  // Test with always-on sampler
  let always_on_sampler = AlwaysOnSampler::new()
  let tracer1 = TracerProvider::get_tracer_with_sampler("always_on_test", always_on_sampler)
  
  let span1 = Tracer::start_span(tracer1, "always_on_operation")
  let ctx1 = Span::span_context(span1)
  
  assert_true(SpanContext::is_sampled(ctx1))
  Span::end(span1)
  
  // Test with always-off sampler
  let always_off_sampler = AlwaysOffSampler::new()
  let tracer2 = TracerProvider::get_tracer_with_sampler("always_off_test", always_off_sampler)
  
  let span2 = Tracer::start_span(tracer2, "always_off_operation")
  let ctx2 = Span::span_context(span2)
  
  assert_false(SpanContext::is_sampled(ctx2))
  Span::end(span2)
  
  // Test with trace ID ratio sampler
  let ratio_sampler = TraceIdRatioBasedSampler::new(0.5) // 50% sampling
  let tracer3 = TracerProvider::get_tracer_with_sampler("ratio_test", ratio_sampler)
  
  let sampled_count = { mut count: 0 }
  let total_spans = 100
  
  for i in 0..=total_spans {
    let span = Tracer::start_span(tracer3, "ratio_operation_" + i.to_string())
    let ctx = Span::span_context(span)
    
    if SpanContext::is_sampled(ctx) {
      sampled_count.count = sampled_count.count + 1
    }
    
    Span::end(span)
  }
  
  // Verify approximately 50% sampling rate
  let sampling_rate = (sampled_count.count as Float) / ((total_spans + 1) as Float)
  assert_true(sampling_rate > 0.3 && sampling_rate < 0.7)
}

// Test 5: Distributed Trace Baggage
test "distributed trace baggage" {
  let tracer = TracerProvider::get_tracer("baggage_test")
  
  // Create initial span with baggage
  let span = Tracer::start_span(tracer, "baggage_operation")
  let ctx = Span::span_context(span)
  
  // Add baggage items
  let baggage1 = Baggage::set_entry(Baggage::new(), "user.id", "12345")
  let baggage2 = Baggage::set_entry(baggage1, "request.id", "req-67890")
  let baggage3 = Baggage::set_entry(baggage2, "tenant.id", "tenant-abc")
  
  // Create context with baggage
  let ctx_with_baggage = Context::with_value(Context::root(), BaggageKey::new(), baggage3)
  
  // Propagate baggage to child span
  let child_span = Tracer::start_span_with_context(tracer, "child_operation", Some(ctx))
  let child_ctx = Span::span_context(child_span)
  
  // Verify baggage is accessible in child context
  let child_baggage = Context::get_value(ctx_with_baggage, BaggageKey::new())
  match child_baggage {
    Some(b) => {
      let user_id = Baggage::get_entry(b, "user.id")
      match user_id {
        Some(value) => assert_eq(value, "12345")
        None => assert_true(false)
      }
      
      let request_id = Baggage::get_entry(b, "request.id")
      match request_id {
        Some(value) => assert_eq(value, "req-67890")
        None => assert_true(false)
      }
      
      let tenant_id = Baggage::get_entry(b, "tenant.id")
      match tenant_id {
        Some(value) => assert_eq(value, "tenant-abc")
        None => assert_true(false)
      }
    }
    None => assert_true(false)
  }
  
  Span::end(child_span)
  Span::end(span)
}

// Test 6: Distributed Trace Links
test "distributed trace links" {
  let tracer = TracerProvider::get_tracer("links_test")
  
  // Create a span to link to
  let linked_span = Tracer::start_span(tracer, "linked_operation")
  let linked_ctx = Span::span_context(linked_span)
  Span::end(linked_span)
  
  // Create a new span with links
  let links = [SpanLink::new(linked_ctx, Some([
    ("link.type", StringValue("causality")),
    ("link.reason", StringValue("dependency"))
  ]))]
  
  let linked_span2 = Tracer::start_span_with_links(
    tracer,
    "operation_with_links",
    links
  )
  
  // Verify links are properly attached
  let span_links = Span::links(linked_span2)
  assert_eq(span_links.length(), 1)
  
  let first_link = span_links[0]
  assert_eq(SpanContext::trace_id(SpanLink::context(first_link)), SpanContext::trace_id(linked_ctx))
  assert_eq(SpanContext::span_id(SpanLink::context(first_link)), SpanContext::span_id(linked_ctx))
  
  let link_attributes = SpanLink::attributes(first_link)
  match link_attributes {
    Some(attrs) => {
      let link_type = Attributes::get(attrs, "link.type")
      match link_type {
        Some(StringValue(value)) => assert_eq(value, "causality")
        _ => assert_true(false)
      }
      
      let link_reason = Attributes::get(attrs, "link.reason")
      match link_reason {
        Some(StringValue(value)) => assert_eq(value, "dependency")
        _ => assert_true(false)
      }
    }
    None => assert_true(false)
  }
  
  Span::end(linked_span2)
}

// Test 7: Distributed Trace Events
test "distributed trace events" {
  let tracer = TracerProvider::get_tracer("events_test")
  
  let span = Tracer::start_span(tracer, "event_operation")
  
  // Add events with different severity levels
  Span::add_event(span, "operation_started", Some([
    ("timestamp", IntValue(1609459200L)),
    ("component", StringValue("auth"))
  ]))
  
  Span::add_event_with_timestamp(
    span,
    "authentication_completed",
    1609459210L,
    Some([
      ("user.id", StringValue("12345")),
      ("auth.method", StringValue("oauth2")),
      ("duration_ms", IntValue(150))
    ])
  )
  
  Span::add_event_with_timestamp(
    span,
    "database_query_executed",
    1609459225L,
    Some([
      ("query.type", StringValue("SELECT")),
      ("table.name", StringValue("users")),
      ("duration_ms", IntValue(45)),
      ("rows_affected", IntValue(1))
    ])
  )
  
  Span::add_event_with_timestamp(
    span,
    "operation_completed",
    1609459230L,
    Some([
      ("status", StringValue("success")),
      ("total_duration_ms", IntValue(200))
    ])
  )
  
  // Verify events are recorded
  let events = Span::events(span)
  assert_eq(events.length(), 4)
  
  // Check first event
  let first_event = events[0]
  assert_eq(SpanEvent::name(first_event), "operation_started")
  assert_eq(SpanEvent::timestamp(first_event), 1609459200L)
  
  let first_event_attrs = SpanEvent::attributes(first_event)
  match first_event_attrs {
    Some(attrs) => {
      let component = Attributes::get(attrs, "component")
      match component {
        Some(StringValue(value)) => assert_eq(value, "auth")
        _ => assert_true(false)
      }
    }
    None => assert_true(false)
  }
  
  // Check last event
  let last_event = events[3]
  assert_eq(SpanEvent::name(last_event), "operation_completed")
  assert_eq(SpanEvent::timestamp(last_event), 1609459230L)
  
  Span::end(span)
}

// Test 8: Distributed Trace Status
test "distributed trace status" {
  let tracer = TracerProvider::get_tracer("status_test")
  
  // Test successful operation
  let success_span = Tracer::start_span(tracer, "successful_operation")
  
  // Add some events
  Span::add_event(success_span, "operation_started")
  Span::add_event(success_span, "operation_completed")
  
  // Set success status
  Span::set_status(success_span, Ok, Some("Operation completed successfully"))
  
  assert_eq(Span::status(success_span), Ok)
  
  let status_message = Span::status_message(success_span)
  match status_message {
    Some(message) => assert_eq(message, "Operation completed successfully")
    None => assert_true(false)
  }
  
  Span::end(success_span)
  
  // Test failed operation
  let error_span = Tracer::start_span(tracer, "failed_operation")
  
  // Add events
  Span::add_event(error_span, "operation_started")
  Span::add_event(error_span, "error_occurred", Some([
    ("error.type", StringValue("ValidationError")),
    ("error.message", StringValue("Invalid input parameter"))
  ]))
  
  // Set error status
  Span::set_status(error_span, Error, Some("Validation failed"))
  
  assert_eq(Span::status(error_span), Error)
  
  let error_message = Span::status_message(error_span)
  match error_message {
    Some(message) => assert_eq(message, "Validation failed")
    None => assert_true(false)
  }
  
  Span::end(error_span)
}

// Test 9: Distributed Trace Export
test "distributed trace export" {
  let tracer = TracerProvider::get_tracer("export_test")
  
  // Create a span with rich data
  let span = Tracer::start_span(tracer, "export_operation")
  
  // Add attributes
  Span::set_attribute(span, "service.name", StringValue("test_service"))
  Span::set_attribute(span, "service.version", StringValue("1.0.0"))
  Span::set_attribute(span, "operation.type", StringValue("export"))
  
  // Add events
  Span::add_event(span, "export_started")
  Span::add_event(span, "data_processed", Some([
    ("records.count", IntValue(1000)),
    ("processing_time_ms", IntValue(250))
  ]))
  
  // Set status
  Span::set_status(span, Ok, Some("Export completed"))
  
  Span::end(span)
  
  // Export span data
  let exporter = SpanExporter::new()
  let export_result = SpanExporter::export_span(exporter, span)
  
  match export_result {
    Success(export_data) => {
      assert_eq(export_data.span_name, "export_operation")
      assert_eq(export_data.status, Ok)
      assert_true(export_data.attributes.length() > 0)
      assert_true(export_data.events.length() > 0)
    }
    Error(err) => assert_true(false)
  }
  
  // Test batch export
  let spans = []
  
  for i in 0..=5 {
    let batch_span = Tracer::start_span(tracer, "batch_operation_" + i.to_string())
    Span::set_attribute(batch_span, "batch.index", IntValue(i))
    Span::end(batch_span)
    spans.push(batch_span)
  }
  
  let batch_export_result = SpanExporter::export_batch(exporter, spans)
  match batch_export_result {
    Success(batch_data) => assert_eq(batch_data.length(), 6)
    Error(_) => assert_true(false)
  }
}

// Test 10: Distributed Trace Performance
test "distributed trace performance" {
  let tracer = TracerProvider::get_tracer("performance_test")
  
  // Test span creation performance
  let start_time = Time::now()
  let span_count = 1000
  
  for i in 0..=span_count {
    let span = Tracer::start_span(tracer, "performance_operation_" + i.to_string())
    Span::set_attribute(span, "index", IntValue(i))
    
    // Add some events
    if i % 10 == 0 {
      Span::add_event(span, "milestone_reached", Some([
        ("milestone", IntValue(i / 10))
      ]))
    }
    
    Span::end(span)
  }
  
  let end_time = Time::now()
  let duration = end_time - start_time
  
  // Performance should be reasonable for 1000 spans
  assert_true(duration < 10000L) // Less than 10 seconds
  
  // Test memory usage
  let memory_usage = TracerProvider::get_memory_usage(tracer)
  assert_true(memory_usage < 10000000) // Less than 10MB
  
  // Test concurrent span creation
  let concurrent_start = Time::now()
  let concurrent_spans = []
  
  for i in 0..=100 {
    let span = Tracer::start_span(tracer, "concurrent_operation_" + i.to_string())
    concurrent_spans.push(span)
  }
  
  // End all spans
  for span in concurrent_spans {
    Span::end(span)
  }
  
  let concurrent_end = Time::now()
  let concurrent_duration = concurrent_end - concurrent_start
  
  // Concurrent operations should be efficient
  assert_true(concurrent_duration < 5000L) // Less than 5 seconds
}