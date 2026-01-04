// Azimuth Telemetry System - Distributed Tracing Consistency Tests
// This file contains comprehensive test cases for distributed tracing consistency

// Test 1: Cross-Service Trace Context Propagation
test "cross-service trace context propagation" {
  // Initialize tracing provider
  let tracer_provider = TracerProvider::new()
  let tracer = TracerProvider::get_tracer(tracer_provider, "consistency_tracer")
  
  // Create root span for the request
  let root_span = Tracer::start_span(tracer, "user_request")
  let root_ctx = Span::span_context(root_span)
  let root_trace_id = SpanContext::trace_id(root_ctx)
  
  // Simulate service A receiving the request
  let service_a_span = Tracer::start_span_with_context(
    tracer, 
    "service_a.process", 
    Some(root_ctx),
    Some(Server)
  )
  
  // Service A calls Service B
  let service_a_ctx = Span::span_context(service_a_span)
  let service_b_span = Tracer::start_span_with_context(
    tracer,
    "service_b.process",
    Some(service_a_ctx),
    Some(Client)
  )
  
  // Service B calls Service C
  let service_b_ctx = Span::span_context(service_b_span)
  let service_c_span = Tracer::start_span_with_context(
    tracer,
    "service_c.process",
    Some(service_b_ctx),
    Some(Client)
  )
  
  // Verify trace context consistency across all services
  let service_c_ctx = Span::span_context(service_c_span)
  
  // All spans should have the same trace ID
  assert_eq(SpanContext::trace_id(root_ctx), root_trace_id)
  assert_eq(SpanContext::trace_id(service_a_ctx), root_trace_id)
  assert_eq(SpanContext::trace_id(service_b_ctx), root_trace_id)
  assert_eq(SpanContext::trace_id(service_c_ctx), root_trace_id)
  
  // Each span should have a unique span ID
  assert_not_eq(SpanContext::span_id(root_ctx), SpanContext::span_id(service_a_ctx))
  assert_not_eq(SpanContext::span_id(service_a_ctx), SpanContext::span_id(service_b_ctx))
  assert_not_eq(SpanContext::span_id(service_b_ctx), SpanContext::span_id(service_c_ctx))
  
  // Verify parent-child relationships
  assert_eq(Span::parent_span_id(service_a_span), Some(SpanContext::span_id(root_ctx)))
  assert_eq(Span::parent_span_id(service_b_span), Some(SpanContext::span_id(service_a_ctx)))
  assert_eq(Span::parent_span_id(service_c_span), Some(SpanContext::span_id(service_b_ctx)))
  
  // End all spans
  Span::end(service_c_span)
  Span::end(service_b_span)
  Span::end(service_a_span)
  Span::end(root_span)
}

// Test 2: Distributed Trace Baggage Propagation
test "distributed trace baggage propagation" {
  let tracer_provider = TracerProvider::new()
  let tracer = TracerProvider::get_tracer(tracer_provider, "baggage_tracer")
  
  // Create root span with initial baggage
  let root_span = Tracer::start_span(tracer, "init_request")
  let root_ctx = Span::span_context(root_span)
  
  // Add baggage items
  let baggage1 = Baggage::new()
  let baggage_with_user = Baggage::set_entry(baggage1, "user.id", "12345")
  let baggage_with_tenant = Baggage::set_entry(baggage_with_user, "tenant.id", "tenant-abc")
  let baggage_with_request_id = Baggage::set_entry(baggage_with_tenant, "request.id", "req-789")
  
  // Attach baggage to context
  let ctx_with_baggage = Context::with_value(
    Context::root(),
    ContextKey::new("baggage"),
    baggage_with_request_id
  )
  
  // Simulate service chain with baggage propagation
  let service_a_span = Tracer::start_span_with_context(
    tracer,
    "service_a.process",
    Some(root_ctx),
    Some(Server)
  )
  
  // Service A should be able to access baggage
  let service_a_baggage = Context::get(ctx_with_baggage, ContextKey::new("baggage"))
  match service_a_baggage {
    Some(baggage) => {
      match Baggage::get_entry(baggage, "user.id") {
        Some(user_id) => assert_eq(user_id, "12345")
        None => assert_true(false)
      }
      match Baggage::get_entry(baggage, "tenant.id") {
        Some(tenant_id) => assert_eq(tenant_id, "tenant-abc")
        None => assert_true(false)
      }
    }
    None => assert_true(false)
  }
  
  // Service B adds more baggage
  let service_b_span = Tracer::start_span_with_context(
    tracer,
    "service_b.process",
    Some(Span::span_context(service_a_span)),
    Some(Client)
  )
  
  let service_b_baggage = match service_a_baggage {
    Some(baggage) => Baggage::set_entry(baggage, "service.b.metadata", "processed")
    None => assert_true(false); Baggage::new()
  }
  
  // Service C should see all baggage items
  let service_c_span = Tracer::start_span_with_context(
    tracer,
    "service_c.process",
    Some(Span::span_context(service_b_span)),
    Some(Client)
  )
  
  // Verify baggage propagation consistency
  match service_b_baggage {
    Some(baggage) => {
      match Baggage::get_entry(baggage, "user.id") {
        Some(user_id) => assert_eq(user_id, "12345")
        None => assert_true(false)
      }
      match Baggage::get_entry(baggage, "tenant.id") {
        Some(tenant_id) => assert_eq(tenant_id, "tenant-abc")
        None => assert_true(false)
      }
      match Baggage::get_entry(baggage, "service.b.metadata") {
        Some(metadata) => assert_eq(metadata, "processed")
        None => assert_true(false)
      }
    }
    None => assert_true(false)
  }
  
  // End spans
  Span::end(service_c_span)
  Span::end(service_b_span)
  Span::end(service_a_span)
  Span::end(root_span)
}

// Test 3: Trace Sampling Consistency
test "trace sampling consistency" {
  // Configure sampling strategy
  let sampler_config = SamplerConfig::new(
    SamplingStrategy::TraceIdRatio,
    0.5, // 50% sampling rate
    Some("test-sampler")
  )
  
  let tracer_provider = TracerProvider::with_sampler(sampler_config)
  let tracer = TracerProvider::get_tracer(tracer_provider, "sampling_tracer")
  
  // Create multiple traces to test sampling consistency
  let sampled_traces = []
  let unsampled_traces = []
  
  // Generate 10 traces
  for i in 0..10 {
    let span_name = "trace_" + i.to_string()
    let span = Tracer::start_span(tracer, span_name)
    let ctx = Span::span_context(span)
    
    if SpanContext::is_sampled(ctx) {
      sampled_traces.push(span)
    } else {
      unsampled_traces.push(span)
    }
    
    Span::end(span)
  }
  
  // With 50% sampling rate and deterministic sampler, we should have approximately equal counts
  assert_true(sampled_traces.length() > 0)
  assert_true(unsampled_traces.length() > 0)
  
  // Test that all spans in a single trace have consistent sampling decisions
  let root_span = Tracer::start_span(tracer, "sampled_trace_root")
  let root_ctx = Span::span_context(root_span)
  let root_sampled = SpanContext::is_sampled(root_ctx)
  
  let child_span = Tracer::start_span_with_context(
    tracer,
    "sampled_trace_child",
    Some(root_ctx),
    Some(Internal)
  )
  let child_ctx = Span::span_context(child_span)
  let child_sampled = SpanContext::is_sampled(child_ctx)
  
  // Parent and child should have the same sampling decision
  assert_eq(root_sampled, child_sampled)
  
  Span::end(child_span)
  Span::end(root_span)
}

// Test 4: Distributed Trace Event Ordering
test "distributed trace event ordering" {
  let tracer_provider = TracerProvider::new()
  let tracer = TracerProvider::get_tracer(tracer_provider, "ordering_tracer")
  
  // Create a trace with multiple services
  let root_span = Tracer::start_span(tracer, "distributed_request")
  let root_start_time = Span::start_time(root_span)
  
  // Simulate service processing with time delays
  let service_a_span = Tracer::start_span_with_context(
    tracer,
    "service_a.process",
    Some(Span::span_context(root_span)),
    Some(Server)
  )
  
  // Add events to service A
  Span::add_event(service_a_span, "service_a.start")
  Span::add_event(service_a_span, "service_a.authenticated")
  Span::add_event(service_a_span, "service_a.processing")
  
  // Service A calls Service B
  let service_b_span = Tracer::start_span_with_context(
    tracer,
    "service_b.process",
    Some(Span::span_context(service_a_span)),
    Some(Client)
  )
  
  // Add events to service B
  Span::add_event(service_b_span, "service_b.start")
  Span::add_event(service_b_span, "service_b.database_query")
  Span::add_event(service_b_span, "service_b.complete")
  
  // Service B calls Service C
  let service_c_span = Tracer::start_span_with_context(
    tracer,
    "service_c.process",
    Some(Span::span_context(service_b_span)),
    Some(Client)
  )
  
  // Add events to service C
  Span::add_event(service_c_span, "service_c.start")
  Span::add_event(service_c_span, "service_c.cache_lookup")
  Span::add_event(service_c_span, "service_c.complete")
  
  // End spans in reverse order
  Span::end(service_c_span)
  Span::end(service_b_span)
  Span::end(service_a_span)
  Span::end(root_span)
  
  // Verify event ordering within each span
  let service_a_events = Span::events(service_a_span)
  assert_eq(service_a_events.length(), 3)
  assert_eq(service_a_events[0].name, "service_a.start")
  assert_eq(service_a_events[1].name, "service_a.authenticated")
  assert_eq(service_a_events[2].name, "service_a.processing")
  
  let service_b_events = Span::events(service_b_span)
  assert_eq(service_b_events.length(), 3)
  assert_eq(service_b_events[0].name, "service_b.start")
  assert_eq(service_b_events[1].name, "service_b.database_query")
  assert_eq(service_b_events[2].name, "service_b.complete")
  
  let service_c_events = Span::events(service_c_span)
  assert_eq(service_c_events.length(), 3)
  assert_eq(service_c_events[0].name, "service_c.start")
  assert_eq(service_c_events[1].name, "service_c.cache_lookup")
  assert_eq(service_c_events[2].name, "service_c.complete")
  
  // Verify span ordering based on timestamps
  assert_true(Span::start_time(root_span) <= Span::start_time(service_a_span))
  assert_true(Span::start_time(service_a_span) <= Span::start_time(service_b_span))
  assert_true(Span::start_time(service_b_span) <= Span::start_time(service_c_span))
  
  // Verify end times are also ordered
  assert_true(Span::end_time(service_c_span) <= Span::end_time(service_b_span))
  assert_true(Span::end_time(service_b_span) <= Span::end_time(service_a_span))
  assert_true(Span::end_time(service_a_span) <= Span::end_time(root_span))
}

// Test 5: Trace Context Injection and Extraction
test "trace context injection and extraction" {
  let tracer_provider = TracerProvider::new()
  let tracer = TracerProvider::get_tracer(tracer_provider, "injection_tracer")
  
  // Create a span
  let span = Tracer::start_span(tracer, "test_span")
  let ctx = Span::span_context(span)
  
  // Get the propagator
  let propagator = TraceContextPropagator::new()
  
  // Create headers carrier
  let carrier = Headers::new()
  
  // Inject context into carrier
  Propagator::inject(propagator, ctx, carrier)
  
  // Verify traceparent header is present
  let traceparent = Headers::get(carrier, "traceparent")
  match traceparent {
    Some(header_value) => {
      // traceparent format: version-trace_id-span_id-flags
      let parts = header_value.split("-")
      assert_eq(parts.length(), 4)
      assert_eq(parts[0], "00") // Version
      assert_eq(parts[1], SpanContext::trace_id(ctx))
      assert_eq(parts[2], SpanContext::span_id(ctx))
      assert_eq(parts[3], "01") // Sampled flag
    }
    None => assert_true(false)
  }
  
  // Verify tracestate header if baggage is present
  let tracestate = Headers::get(carrier, "tracestate")
  // tracestate might not be present if no baggage is set
  
  // Extract context from carrier
  let extracted_ctx = Propagator::extract(propagator, carrier)
  
  // Verify extracted context matches original
  assert_eq(SpanContext::trace_id(extracted_ctx), SpanContext::trace_id(ctx))
  assert_eq(SpanContext::span_id(extracted_ctx), SpanContext::span_id(ctx))
  assert_eq(SpanContext::is_sampled(extracted_ctx), SpanContext::is_sampled(ctx))
  
  Span::end(span)
}

// Test 6: Distributed Trace Consistency Under Failure
test "distributed trace consistency under failure" {
  let tracer_provider = TracerProvider::new()
  let tracer = TracerProvider::get_tracer(tracer_provider, "failure_tracer")
  
  // Create a trace that simulates partial failure
  let root_span = Tracer::start_span(tracer, "request_with_failure")
  let root_ctx = Span::span_context(root_span)
  
  // Service A processes successfully
  let service_a_span = Tracer::start_span_with_context(
    tracer,
    "service_a.process",
    Some(root_ctx),
    Some(Server)
  )
  
  // Service A calls Service B
  let service_b_span = Tracer::start_span_with_context(
    tracer,
    "service_b.process",
    Some(Span::span_context(service_a_span)),
    Some(Client)
  )
  
  // Service B encounters an error
  Span::set_status(service_b_span, Error, Some("Database connection failed"))
  Span::add_event(service_b_span, "error", Some([
    ("error.type", StringValue("database")),
    ("error.message", StringValue("Connection timeout"))
  ]))
  
  // Service B attempts to call Service C but fails
  let service_c_span = Tracer::start_span_with_context(
    tracer,
    "service_c.process",
    Some(Span::span_context(service_b_span)),
    Some(Client)
  )
  
  // Service C never gets called due to failure in B
  Span::set_status(service_c_span, Error, Some("Service C not reached"))
  
  // End spans
  Span::end(service_c_span)
  Span::end(service_b_span)
  
  // Service A continues processing despite B's failure
  Span::add_event(service_a_span, "fallback_activated")
  Span::set_status(service_a_span, Ok, Some("Processed with fallback"))
  
  Span::end(service_a_span)
  
  // Root span reflects partial failure
  Span::set_status(root_span, Error, Some("Partial service failure"))
  Span::end(root_span)
  
  // Verify trace consistency despite failures
  
  // All spans should still have the same trace ID
  assert_eq(SpanContext::trace_id(root_ctx), SpanContext::trace_id(Span::span_context(service_a_span)))
  assert_eq(SpanContext::trace_id(root_ctx), SpanContext::trace_id(Span::span_context(service_b_span)))
  assert_eq(SpanContext::trace_id(root_ctx), SpanContext::trace_id(Span::span_context(service_c_span)))
  
  // Status should reflect the errors
  assert_eq(Span::status(service_b_span), Error)
  assert_eq(Span::status(service_c_span), Error)
  assert_eq(Span::status(service_a_span), Ok) // Recovered
  assert_eq(Span::status(root_span), Error) // Overall failure
  
  // Verify error propagation
  match Span::status_message(service_b_span) {
    Some(message) => assert_eq(message, "Database connection failed")
    None => assert_true(false)
  }
  
  match Span::status_message(service_c_span) {
    Some(message) => assert_eq(message, "Service C not reached")
    None => assert_true(false)
  }
}