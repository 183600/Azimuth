// Azimuth Distributed Tracing Advanced Test Suite
// This file contains comprehensive test cases for advanced distributed tracing scenarios

// Test 1: Cross-Service Trace Propagation
test "cross-service trace propagation" {
  // Initialize providers for different services
  let web_provider = TelemetryProvider::with_resource(Resource::with_attributes(Resource::new(), [
    ("service.name", "web-service"),
    ("service.version", "1.0.0")
  ]))
  
  let api_provider = TelemetryProvider::with_resource(Resource::with_attributes(Resource::new(), [
    ("service.name", "api-service"),
    ("service.version", "2.1.0")
  ]))
  
  let db_provider = TelemetryProvider::with_resource(Resource::with_attributes(Resource::new(), [
    ("service.name", "database-service"),
    ("service.version", "3.5.0")
  ]))
  
  // Get tracers for each service
  let web_tracer = TelemetryProvider::get_tracer(web_provider, "web-tracer")
  let api_tracer = TelemetryProvider::get_tracer(api_provider, "api-tracer")
  let db_tracer = TelemetryProvider::get_tracer(db_provider, "db-tracer")
  
  // Web service creates the root span
  let web_span = Tracer::start_span(web_tracer, "web-request")
  let trace_id = Span::trace_id(web_span)
  let web_span_id = Span::span_id(web_span)
  
  // Web service calls API service
  let api_span_ctx = SpanContext::new(trace_id, "api-span-123", true, "")
  let api_span = Tracer::start_span_with_context(api_tracer, "api-request", api_span_ctx)
  Span::set_parent(api_span, web_span_id)
  let api_span_id = Span::span_id(api_span)
  
  // API service calls database service
  let db_span_ctx = SpanContext::new(trace_id, "db-span-456", true, "")
  let db_span = Tracer::start_span_with_context(db_tracer, "db-query", db_span_ctx)
  Span::set_parent(db_span, api_span_id)
  
  // Add service-specific attributes
  Span::add_attribute(web_span, "http.method", "GET")
  Span::add_attribute(web_span, "http.url", "/api/users")
  
  Span::add_attribute(api_span, "api.method", "GET")
  Span::add_attribute(api_span, "api.resource", "users")
  
  Span::add_attribute(db_span, "db.type", "postgresql")
  Span::add_attribute(db_span, "db.statement", "SELECT * FROM users")
  
  // Add events
  Span::add_event(web_span, "request.received", Some([("user.id", "user123")]))
  Span::add_event(api_span, "service.call", Some([("service", "database")]))
  Span::add_event(db_span, "query.start", Some([("query", "SELECT * FROM users")]))
  Span::add_event(db_span, "query.complete", Some([("rows", "10")]))
  
  // End spans in reverse order (database first)
  Span::end(db_span)
  Span::end(api_span)
  Span::end(web_span)
  
  // Force export
  TelemetryProvider::force_flush(web_provider)
  TelemetryProvider::force_flush(api_provider)
  TelemetryProvider::force_flush(db_provider)
  
  // Verify trace propagation
  let web_spans = TelemetryProvider::get_collected_spans(web_provider)
  let api_spans = TelemetryProvider::get_collected_spans(api_provider)
  let db_spans = TelemetryProvider::get_collected_spans(db_provider)
  
  assert_eq(web_spans.length(), 1)
  assert_eq(api_spans.length(), 1)
  assert_eq(db_spans.length(), 1)
  
  // All spans should have the same trace ID
  assert_eq(web_spans[0].trace_id, trace_id)
  assert_eq(api_spans[0].trace_id, trace_id)
  assert_eq(db_spans[0].trace_id, trace_id)
  
  // Verify parent-child relationships
  assert_eq(api_spans[0].parent_span_id, Some(web_span_id))
  assert_eq(db_spans[0].parent_span_id, Some(api_span_id))
  
  // Verify service-specific attributes
  assert_true(web_spans[0].attributes.contains(("http.method", "GET")))
  assert_true(web_spans[0].attributes.contains(("http.url", "/api/users")))
  
  assert_true(api_spans[0].attributes.contains(("api.method", "GET")))
  assert_true(api_spans[0].attributes.contains(("api.resource", "users")))
  
  assert_true(db_spans[0].attributes.contains(("db.type", "postgresql")))
  assert_true(db_spans[0].attributes.contains(("db.statement", "SELECT * FROM users")))
}

// Test 2: Trace Context Injection and Extraction
test "trace context injection and extraction" {
  let provider = TelemetryProvider::new()
  let tracer = TelemetryProvider::get_tracer(provider, "context_tracer")
  
  // Create a span
  let span = Tracer::start_span(tracer, "context_test_span")
  let span_context = Span::span_context(span)
  
  // Inject context into headers
  let headers = Headers::new()
  TraceContext::inject(headers, span_context)
  
  // Verify headers contain trace context
  assert_true(Headers::contains(headers, "traceparent"))
  assert_true(Headers::contains(headers, "tracestate"))
  
  // Extract context from headers
  let extracted_context = TraceContext::extract(headers)
  
  // Verify extracted context matches original
  assert_true(extracted_context.is_some())
  
  match extracted_context {
    Some(context) => {
      assert_eq(SpanContext::trace_id(context), SpanContext::trace_id(span_context))
      assert_eq(SpanContext::span_id(context), SpanContext::span_id(span_context))
      assert_eq(SpanContext::is_sampled(context), SpanContext::is_sampled(span_context))
    }
    None => assert_true(false)
  }
  
  // Create a new span with extracted context
  let child_span = Tracer::start_span_with_context(tracer, "child_span", extracted_context.unwrap())
  Span::set_parent(child_span, SpanContext::span_id(span_context))
  
  // End spans
  Span::end(child_span)
  Span::end(span)
  
  // Verify trace continuity
  let spans = TelemetryProvider::get_collected_spans(provider)
  assert_eq(spans.length(), 2)
  
  let parent_span = spans.find(fn(s) { s.name == "context_test_span" })
  let child_span_collected = spans.find(fn(s) { s.name == "child_span" })
  
  match (parent_span, child_span_collected) {
    (Some(parent), Some(child)) => {
      assert_eq(parent.trace_id, child.trace_id)
      assert_eq(child.parent_span_id, Some(parent.span_id))
    }
    _ => assert_true(false)
  }
}

// Test 3: Baggage Propagation Across Services
test "baggage propagation across services" {
  // Initialize providers for different services
  let service_a_provider = TelemetryProvider::new()
  let service_b_provider = TelemetryProvider::new()
  let service_c_provider = TelemetryProvider::new()
  
  // Get tracers for each service
  let service_a_tracer = TelemetryProvider::get_tracer(service_a_provider, "service-a-tracer")
  let service_b_tracer = TelemetryProvider::get_tracer(service_b_provider, "service-b-tracer")
  let service_c_tracer = TelemetryProvider::get_tracer(service_c_provider, "service-c-tracer")
  
  // Service A creates initial baggage
  let initial_baggage = Baggage::new()
  let baggage_with_user = Baggage::set_entry(initial_baggage, "user.id", "user123")
  let baggage_with_request = Baggage::set_entry(baggage_with_user, "request.id", "req456")
  
  // Service A creates a span with baggage
  let service_a_span = Tracer::start_span_with_baggage(service_a_tracer, "service-a-operation", baggage_with_request)
  
  // Service A calls Service B, propagating baggage
  let service_b_baggage = Baggage::set_entry(baggage_with_request, "service.a.timestamp", "1640995200")
  let service_b_span = Tracer::start_span_with_baggage(service_b_tracer, "service-b-operation", service_b_baggage)
  Span::set_parent(service_b_span, Span::span_id(service_a_span))
  
  // Service B calls Service C, propagating baggage
  let service_c_baggage = Baggage::set_entry(service_b_baggage, "service.b.timestamp", "1640995250")
  let service_c_span = Tracer::start_span_with_baggage(service_c_tracer, "service-c-operation", service_c_baggage)
  Span::set_parent(service_c_span, Span::span_id(service_b_span))
  
  // Add baggage entries in Service C
  let final_baggage = Baggage::set_entry(service_c_baggage, "service.c.result", "success")
  
  // End spans
  Span::end(service_c_span)
  Span::end(service_b_span)
  Span::end(service_a_span)
  
  // Force export
  TelemetryProvider::force_flush(service_a_provider)
  TelemetryProvider::force_flush(service_b_provider)
  TelemetryProvider::force_flush(service_c_provider)
  
  // Verify baggage propagation
  let service_a_spans = TelemetryProvider::get_collected_spans(service_a_provider)
  let service_b_spans = TelemetryProvider::get_collected_spans(service_b_provider)
  let service_c_spans = TelemetryProvider::get_collected_spans(service_c_provider)
  
  assert_eq(service_a_spans.length(), 1)
  assert_eq(service_b_spans.length(), 1)
  assert_eq(service_c_spans.length(), 1)
  
  // Verify baggage entries in Service C
  let service_c_span = service_c_spans[0]
  let service_c_baggage_collected = Span::get_baggage(service_c_span)
  
  assert_true(service_c_baggage_collected.is_some())
  
  match service_c_baggage_collected {
    Some(baggage) => {
      // Verify original baggage entries
      let user_id = Baggage::get_entry(baggage, "user.id")
      let request_id = Baggage::get_entry(baggage, "request.id")
      let service_a_timestamp = Baggage::get_entry(baggage, "service.a.timestamp")
      let service_b_timestamp = Baggage::get_entry(baggage, "service.b.timestamp")
      let service_c_result = Baggage::get_entry(baggage, "service.c.result")
      
      match (user_id, request_id, service_a_timestamp, service_b_timestamp, service_c_result) {
        (Some(uid), Some(rid), Some(atime), Some(btime), Some(result)) => {
          assert_eq(uid, "user123")
          assert_eq(rid, "req456")
          assert_eq(atime, "1640995200")
          assert_eq(btime, "1640995250")
          assert_eq(result, "success")
        }
        _ => assert_true(false)
      }
    }
    None => assert_true(false)
  }
}

// Test 4: Trace Sampling with Sampling Decision Propagation
test "trace sampling with sampling decision propagation" {
  // Configure providers with different sampling strategies
  let always_on_config = {
    sampling_enabled: true,
    sampling_rate: 1.0, // 100% sampling
    max_spans_per_second: 1000
  }
  
  let probability_config = {
    sampling_enabled: true,
    sampling_rate: 0.5, // 50% sampling
    max_spans_per_second: 1000
  }
  
  let always_off_config = {
    sampling_enabled: true,
    sampling_rate: 0.0, // 0% sampling
    max_spans_per_second: 1000
  }
  
  let always_on_provider = TelemetryProvider::with_config(always_on_config)
  let probability_provider = TelemetryProvider::with_config(probability_config)
  let always_off_provider = TelemetryProvider::with_config(always_off_config)
  
  // Get tracers
  let always_on_tracer = TelemetryProvider::get_tracer(always_on_provider, "always-on-tracer")
  let probability_tracer = TelemetryProvider::get_tracer(probability_provider, "probability-tracer")
  let always_off_tracer = TelemetryProvider::get_tracer(always_off_provider, "always-off-tracer")
  
  // Create spans with different sampling strategies
  let always_on_span = Tracer::start_span(always_on_tracer, "always-on-span")
  let probability_span = Tracer::start_span(probability_tracer, "probability-span")
  let always_off_span = Tracer::start_span(always_off_tracer, "always-off-span")
  
  // Verify sampling decisions
  assert_true(Span::is_sampled(always_on_span)) // Always sampled
  // Probability span may or may not be sampled
  assert_false(Span::is_sampled(always_off_span)) // Never sampled
  
  // Create child spans with parent sampling decision
  let always_on_child = Tracer::start_span_with_parent(always_on_tracer, "always-on-child", always_on_span)
  let probability_child = Tracer::start_span_with_parent(probability_tracer, "probability-child", probability_span)
  let always_off_child = Tracer::start_span_with_parent(always_off_tracer, "always-off-child", always_off_span)
  
  // Child spans should respect parent sampling decision
  assert_true(Span::is_sampled(always_on_child))
  assert_eq(Span::is_sampled(probability_child), Span::is_sampled(probability_span))
  assert_false(Span::is_sampled(always_off_child))
  
  // End spans
  Span::end(always_on_child)
  Span::end(always_on_span)
  Span::end(probability_child)
  Span::end(probability_span)
  Span::end(always_off_child)
  Span::end(always_off_span)
  
  // Force export
  TelemetryProvider::force_flush(always_on_provider)
  TelemetryProvider::force_flush(probability_provider)
  TelemetryProvider::force_flush(always_off_provider)
  
  // Verify sampling results
  let always_on_spans = TelemetryProvider::get_collected_spans(always_on_provider)
  let probability_spans = TelemetryProvider::get_collected_spans(probability_provider)
  let always_off_spans = TelemetryProvider::get_collected_spans(always_off_provider)
  
  assert_eq(always_on_spans.length(), 2) // Both spans should be sampled
  assert_eq(always_off_spans.length(), 0) // No spans should be sampled
  
  // Probability spans depend on random sampling, but if parent is sampled, child should be sampled
  if probability_spans.length() > 0 {
    // If parent was sampled, both should be present
    assert_eq(probability_spans.length(), 2)
    
    let parent = probability_spans.find(fn(s) { s.name == "probability-span" })
    let child = probability_spans.find(fn(s) { s.name == "probability-child" })
    
    match (parent, child) {
      (Some(p), Some(c)) => {
        assert_eq(p.trace_id, c.trace_id)
        assert_eq(c.parent_span_id, Some(p.span_id))
      }
      _ => assert_true(false)
    }
  }
}

// Test 5: Trace Links and References
test "trace links and references" {
  let provider = TelemetryProvider::new()
  let tracer = TelemetryProvider::get_tracer(provider, "links_tracer")
  
  // Create a parent span
  let parent_span = Tracer::start_span(tracer, "parent-operation")
  let parent_context = Span::span_context(parent_span)
  
  // Create a related span (not a child)
  let related_span = Tracer::start_span(tracer, "related-operation")
  let related_context = Span::span_context(related_span)
  
  // End the related span
  Span::end(related_span)
  
  // Create a new span that links to the related span
  let linked_span = Tracer::start_span_with_links(tracer, "linked-operation", [related_context])
  Span::set_parent(linked_span, Span::span_id(parent_span))
  
  // Add attributes to spans
  Span::add_attribute(parent_span, "operation.type", "parent")
  Span::add_attribute(linked_span, "operation.type", "linked")
  
  // End spans
  Span::end(linked_span)
  Span::end(parent_span)
  
  // Force export
  TelemetryProvider::force_flush(provider)
  
  // Verify links were recorded
  let spans = TelemetryProvider::get_collected_spans(provider)
  assert_eq(spans.length(), 3)
  
  let parent_span_collected = spans.find(fn(s) { s.name == "parent-operation" })
  let related_span_collected = spans.find(fn(s) { s.name == "related-operation" })
  let linked_span_collected = spans.find(fn(s) { s.name == "linked-operation" })
  
  match (parent_span_collected, related_span_collected, linked_span_collected) {
    (Some(parent), Some(related), Some(linked)) => {
      // Verify parent-child relationship
      assert_eq(linked.parent_span_id, Some(parent.span_id))
      
      // Verify link
      assert_eq(linked.links.length(), 1)
      assert_eq(linked.links[0].trace_id, related.trace_id)
      assert_eq(linked.links[0].span_id, related.span_id)
      
      // Verify attributes
      assert_true(parent.attributes.contains(("operation.type", "parent")))
      assert_true(linked.attributes.contains(("operation.type", "linked")))
    }
    _ => assert_true(false)
  }
}

// Test 6: Trace State Propagation
test "trace state propagation" {
  let provider = TelemetryProvider::new()
  let tracer = TelemetryProvider::get_tracer(provider, "trace_state_tracer")
  
  // Create a span with trace state
  let trace_state = TraceState::new()
  let updated_trace_state = TraceState::set(trace_state, "vendor1", "value1")
  let final_trace_state = TraceState::set(updated_trace_state, "vendor2", "value2")
  
  let span_context = SpanContext::new("trace-123", "span-456", true, final_trace_state)
  let span = Tracer::start_span_with_context(tracer, "trace-state-span", span_context)
  
  // Verify trace state
  let current_trace_state = Span::get_trace_state(span)
  assert_true(current_trace_state.is_some())
  
  match current_trace_state {
    Some(state) => {
      let vendor1_value = TraceState::get(state, "vendor1")
      let vendor2_value = TraceState::get(state, "vendor2")
      
      match (vendor1_value, vendor2_value) {
        (Some(v1), Some(v2)) => {
          assert_eq(v1, "value1")
          assert_eq(v2, "value2")
        }
        _ => assert_true(false)
      }
    }
    None => assert_true(false)
  }
  
  // Create a child span
  let child_span = Tracer::start_span_with_parent(tracer, "child-span", span)
  
  // Child should inherit trace state
  let child_trace_state = Span::get_trace_state(child_span)
  assert_true(child_trace_state.is_some())
  
  match child_trace_state {
    Some(state) => {
      let vendor1_value = TraceState::get(state, "vendor1")
      let vendor2_value = TraceState::get(state, "vendor2")
      
      match (vendor1_value, vendor2_value) {
        (Some(v1), Some(v2)) => {
          assert_eq(v1, "value1")
          assert_eq(v2, "value2")
        }
        _ => assert_true(false)
      }
    }
    None => assert_true(false)
  }
  
  // Update trace state in child
  let updated_child_trace_state = TraceState::set(child_trace_state.unwrap(), "vendor3", "value3")
  Span::set_trace_state(child_span, updated_child_trace_state)
  
  // End spans
  Span::end(child_span)
  Span::end(span)
  
  // Force export
  TelemetryProvider::force_flush(provider)
  
  // Verify trace state was preserved
  let spans = TelemetryProvider::get_collected_spans(provider)
  assert_eq(spans.length(), 2)
  
  let parent_span_collected = spans.find(fn(s) { s.name == "trace-state-span" })
  let child_span_collected = spans.find(fn(s) { s.name == "child-span" })
  
  match (parent_span_collected, child_span_collected) {
    (Some(parent), Some(child)) => {
      // Verify parent trace state
      let parent_trace_state = Span::get_trace_state(parent)
      assert_true(parent_trace_state.is_some())
      
      // Verify child trace state
      let child_trace_state = Span::get_trace_state(child)
      assert_true(child_trace_state.is_some())
      
      match child_trace_state {
        Some(state) => {
          let vendor3_value = TraceState::get(state, "vendor3")
          match vendor3_value {
            Some(v3) => assert_eq(v3, "value3")
            None => assert_true(false)
          }
        }
        None => assert_true(false)
      }
    }
    _ => assert_true(false)
  }
}

// Test 7: Distributed Trace Aggregation
test "distributed trace aggregation" {
  // Initialize providers for different services
  let api_provider = TelemetryProvider::new()
  let auth_provider = TelemetryProvider::new()
  let user_provider = TelemetryProvider::new()
  let payment_provider = TelemetryProvider::new()
  
  // Get tracers
  let api_tracer = TelemetryProvider::get_tracer(api_provider, "api-tracer")
  let auth_tracer = TelemetryProvider::get_tracer(auth_provider, "auth-tracer")
  let user_tracer = TelemetryProvider::get_tracer(user_provider, "user-tracer")
  let payment_tracer = TelemetryProvider::get_tracer(payment_provider, "payment-tracer")
  
  // API service creates the root span
  let api_span = Tracer::start_span(api_tracer, "api-request")
  let trace_id = Span::trace_id(api_span)
  
  // API calls auth service
  let auth_span_ctx = SpanContext::new(trace_id, "auth-span-123", true, "")
  let auth_span = Tracer::start_span_with_context(auth_tracer, "auth-check", auth_span_ctx)
  Span::set_parent(auth_span, Span::span_id(api_span))
  
  // API calls user service
  let user_span_ctx = SpanContext::new(trace_id, "user-span-456", true, "")
  let user_span = Tracer::start_span_with_context(user_tracer, "user-fetch", user_span_ctx)
  Span::set_parent(user_span, Span::span_id(api_span))
  
  // User service calls payment service
  let payment_span_ctx = SpanContext::new(trace_id, "payment-span-789", true, "")
  let payment_span = Tracer::start_span_with_context(payment_tracer, "payment-process", payment_span_ctx)
  Span::set_parent(payment_span, Span::span_id(user_span))
  
  // Add timing information
  let api_start = 1640995200000 // ms
  let auth_start = api_start + 10
  let auth_end = auth_start + 50
  let user_start = api_start + 20
  let user_end = user_start + 100
  let payment_start = user_start + 30
  let payment_end = payment_start + 75
  
  // Add timing events
  Span::add_event(api_span, "auth.start", Some([("timestamp", auth_start.to_string())]))
  Span::add_event(api_span, "user.start", Some([("timestamp", user_start.to_string())]))
  Span::add_event(auth_span, "auth.start", Some([("timestamp", auth_start.to_string())]))
  Span::add_event(auth_span, "auth.end", Some([("timestamp", auth_end.to_string())]))
  Span::add_event(user_span, "user.start", Some([("timestamp", user_start.to_string())]))
  Span::add_event(user_span, "payment.start", Some([("timestamp", payment_start.to_string())]))
  Span::add_event(user_span, "user.end", Some([("timestamp", user_end.to_string())]))
  Span::add_event(payment_span, "payment.start", Some([("timestamp", payment_start.to_string())]))
  Span::add_event(payment_span, "payment.end", Some([("timestamp", payment_end.to_string())]))
  
  // End spans
  Span::end(payment_span)
  Span::end(user_span)
  Span::end(auth_span)
  Span::end(api_span)
  
  // Force export
  TelemetryProvider::force_flush(api_provider)
  TelemetryProvider::force_flush(auth_provider)
  TelemetryProvider::force_flush(user_provider)
  TelemetryProvider::force_flush(payment_provider)
  
  // Collect all spans
  let api_spans = TelemetryProvider::get_collected_spans(api_provider)
  let auth_spans = TelemetryProvider::get_collected_spans(auth_provider)
  let user_spans = TelemetryProvider::get_collected_spans(user_provider)
  let payment_spans = TelemetryProvider::get_collected_spans(payment_provider)
  
  // Aggregate all spans
  let all_spans = api_spans + auth_spans + user_spans + payment_spans
  
  // Verify trace aggregation
  assert_eq(all_spans.length(), 4)
  
  // All spans should have the same trace ID
  let all_same_trace_id = all_spans.all(fn(s) { s.trace_id == trace_id })
  assert_true(all_same_trace_id)
  
  // Calculate trace duration
  let trace_start = api_start
  let trace_end = payment_end
  let trace_duration = trace_end - trace_start
  
  // Verify service durations
  let auth_duration = auth_end - auth_start
  let user_duration = user_end - user_start
  let payment_duration = payment_end - payment_start
  
  assert_eq(auth_duration, 50)
  assert_eq(user_duration, 100)
  assert_eq(payment_duration, 75)
  
  // Verify trace duration is the maximum of all service durations
  assert_eq(trace_duration, 175) // 100 (user) + 75 (payment)
  
  // Create a trace summary
  let trace_summary = {
    trace_id: trace_id,
    span_count: all_spans.length(),
    services: ["api", "auth", "user", "payment"],
    duration_ms: trace_duration,
    status: "completed"
  }
  
  assert_eq(trace_summary.span_count, 4)
  assert_eq(trace_summary.services.length(), 4)
  assert_eq(trace_summary.duration_ms, 175)
}

// Test 8: Trace Filtering and Analysis
test "trace filtering and analysis" {
  let provider = TelemetryProvider::new()
  let tracer = TelemetryProvider::get_tracer(provider, "filtering_tracer")
  
  // Create spans with different attributes
  let success_span = Tracer::start_span(tracer, "success-operation")
  Span::add_attribute(success_span, "status", "success")
  Span::add_attribute(success_span, "service", "api")
  Span::set_status(success_span, Ok, None)
  
  let error_span = Tracer::start_span(tracer, "error-operation")
  Span::add_attribute(error_span, "status", "error")
  Span::add_attribute(error_span, "service", "database")
  Span::set_status(error_span, Error, Some("Connection failed"))
  
  let timeout_span = Tracer::start_span(tracer, "timeout-operation")
  Span::add_attribute(timeout_span, "status", "timeout")
  Span::add_attribute(timeout_span, "service", "cache")
  Span::set_status(timeout_span, Error, Some("Operation timed out"))
  
  let warning_span = Tracer::start_span(tracer, "warning-operation")
  Span::add_attribute(warning_span, "status", "warning")
  Span::add_attribute(warning_span, "service", "auth")
  Span::set_status(warning_span, Ok, Some("Deprecated API used"))
  
  // End spans
  Span::end(success_span)
  Span::end(error_span)
  Span::end(timeout_span)
  Span::end(warning_span)
  
  // Force export
  TelemetryProvider::force_flush(provider)
  
  // Get all spans
  let spans = TelemetryProvider::get_collected_spans(provider)
  assert_eq(spans.length(), 4)
  
  // Filter by status
  let success_spans = spans.filter(fn(s) { s.status == Ok })
  let error_spans = spans.filter(fn(s) { s.status == Error })
  
  assert_eq(success_spans.length(), 2)
  assert_eq(error_spans.length(), 2)
  
  // Filter by service
  let api_spans = spans.filter(fn(s) { s.attributes.contains(("service", "api")) })
  let database_spans = spans.filter(fn(s) { s.attributes.contains(("service", "database")) })
  let cache_spans = spans.filter(fn(s) { s.attributes.contains(("service", "cache")) })
  let auth_spans = spans.filter(fn(s) { s.attributes.contains(("service", "auth")) })
  
  assert_eq(api_spans.length(), 1)
  assert_eq(database_spans.length(), 1)
  assert_eq(cache_spans.length(), 1)
  assert_eq(auth_spans.length(), 1)
  
  // Filter by custom attribute
  let status_error_spans = spans.filter(fn(s) { s.attributes.contains(("status", "error")) })
  let status_timeout_spans = spans.filter(fn(s) { s.attributes.contains(("status", "timeout")) })
  
  assert_eq(status_error_spans.length(), 1)
  assert_eq(status_timeout_spans.length(), 1)
  
  // Analyze trace patterns
  let status_distribution = {
    success: success_spans.length(),
    error: error_spans.length()
  }
  
  let service_distribution = {
    api: api_spans.length(),
    database: database_spans.length(),
    cache: cache_spans.length(),
    auth: auth_spans.length()
  }
  
  assert_eq(status_distribution.success, 2)
  assert_eq(status_distribution.error, 2)
  
  assert_eq(service_distribution.api, 1)
  assert_eq(service_distribution.database, 1)
  assert_eq(service_distribution.cache, 1)
  assert_eq(service_distribution.auth, 1)
  
  // Create trace analysis report
  let analysis_report = {
    total_spans: spans.length(),
    status_distribution: status_distribution,
    service_distribution: service_distribution,
    error_rate: error_spans.length().to_float() / spans.length().to_float()
  }
  
  assert_eq(analysis_report.total_spans, 4)
  assert_eq(analysis_report.error_rate, 0.5)
}

// Test 9: Trace Performance Analysis
test "trace performance analysis" {
  let provider = TelemetryProvider::new()
  let tracer = TelemetryProvider::get_tracer(provider, "performance_tracer")
  
  // Create spans with different durations
  let fast_span = Tracer::start_span(tracer, "fast-operation")
  Span::add_event(fast_span, "start", Some([("timestamp", "1000")]))
  Span::add_event(fast_span, "end", Some([("timestamp", "1050")]))
  
  let medium_span = Tracer::start_span(tracer, "medium-operation")
  Span::add_event(medium_span, "start", Some([("timestamp", "2000")]))
  Span::add_event(medium_span, "end", Some([("timestamp", "2200")]))
  
  let slow_span = Tracer::start_span(tracer, "slow-operation")
  Span::add_event(slow_span, "start", Some([("timestamp", "3000")]))
  Span::add_event(slow_span, "end", Some([("timestamp", "3500")]))
  
  // End spans
  Span::end(fast_span)
  Span::end(medium_span)
  Span::end(slow_span)
  
  // Force export
  TelemetryProvider::force_flush(provider)
  
  // Get spans
  let spans = TelemetryProvider::get_collected_spans(provider)
  assert_eq(spans.length(), 3)
  
  // Calculate durations
  let calculate_duration = fn(span: Span) {
    let start_event = span.events.find(fn(e) { e.name == "start" })
    let end_event = span.events.find(fn(e) { e.name == "end" })
    
    match (start_event, end_event) {
      (Some(start), Some(end)) => {
        let start_time = start.attributes.find(fn(a) { a.0 == "timestamp" })
        let end_time = end.attributes.find(fn(a) { a.0 == "timestamp" })
        
        match (start_time, end_time) {
          (Some((_, start)), Some((_, end))) => {
            Some(end.to_int() - start.to_int())
          }
          _ => None
        }
      }
      _ => None
    }
  }
  
  let fast_duration = calculate_duration(fast_span)
  let medium_duration = calculate_duration(medium_span)
  let slow_duration = calculate_duration(slow_span)
  
  match (fast_duration, medium_duration, slow_duration) {
    (Some(fast), Some(medium), Some(slow)) => {
      assert_eq(fast, 50)
      assert_eq(medium, 200)
      assert_eq(slow, 500)
      
      // Calculate performance metrics
      let durations = [fast, medium, slow]
      let total_duration = durations.reduce(fn(acc, d) { acc + d }, 0)
      let average_duration = total_duration / durations.length()
      let min_duration = durations.reduce(fn(acc, d) { if d < acc { d } else { acc }, durations[0])
      let max_duration = durations.reduce(fn(acc, d) { if d > acc { d } else { acc }, durations[0])
      
      assert_eq(average_duration, 250)
      assert_eq(min_duration, 50)
      assert_eq(max_duration, 500)
      
      // Create performance report
      let performance_report = {
        span_count: durations.length(),
        total_duration: total_duration,
        average_duration: average_duration,
        min_duration: min_duration,
        max_duration: max_duration,
        slowest_span: "slow-operation",
        fastest_span: "fast-operation"
      }
      
      assert_eq(performance_report.span_count, 3)
      assert_eq(performance_report.total_duration, 750)
      assert_eq(performance_report.average_duration, 250)
    }
    _ => assert_true(false)
  }
}

// Test 10: Trace Correlation with Logs and Metrics
test "trace correlation with logs and metrics" {
  let provider = TelemetryProvider::new()
  let tracer = TelemetryProvider::get_tracer(provider, "correlation_tracer")
  let meter = TelemetryProvider::get_meter(provider, "correlation_meter")
  let logger = TelemetryProvider::get_logger(provider, "correlation_logger")
  
  // Create a span
  let span = Tracer::start_span(tracer, "correlation-operation")
  let trace_id = Span::trace_id(span)
  let span_id = Span::span_id(span)
  
  // Create metrics correlated with the span
  let counter = Meter::create_counter(meter, "operation_counter")
  let histogram = Meter::create_histogram(meter, "operation_duration")
  
  let attrs = Attributes::new()
  Attributes::set(attrs, "trace_id", trace_id)
  Attributes::set(attrs, "span_id", span_id)
  
  Counter::add(counter, 1.0, Some(attrs))
  Histogram::record(histogram, 100.0, Some(attrs))
  
  // Create logs correlated with the span
  let info_log = Logger::create_log_record(logger, Info, "Operation started")
  LogRecord::add_attribute(info_log, "trace_id", trace_id)
  LogRecord::add_attribute(info_log, "span_id", span_id)
  
  let warning_log = Logger::create_log_record(logger, Warning, "Operation taking longer than expected")
  LogRecord::add_attribute(warning_log, "trace_id", trace_id)
  LogRecord::add_attribute(warning_log, "span_id", span_id)
  
  let error_log = Logger::create_log_record(logger, Error, "Operation failed")
  LogRecord::add_attribute(error_log, "trace_id", trace_id)
  LogRecord::add_attribute(error_log, "span_id", span_id)
  
  // Add events to span
  Span::add_event(span, "operation.started", Some([
    ("log.correlation", "info_log")
  ]))
  
  Span::add_event(span, "operation.warning", Some([
    ("log.correlation", "warning_log")
  ]))
  
  Span::add_event(span, "operation.failed", Some([
    ("log.correlation", "error_log")
  ]))
  
  // Set span status
  Span::set_status(span, Error, Some("Operation failed"))
  
  // End span
  Span::end(span)
  
  // Emit logs
  Logger::emit(logger, info_log)
  Logger::emit(logger, warning_log)
  Logger::emit(logger, error_log)
  
  // Force export
  TelemetryProvider::force_flush(provider)
  
  // Get all telemetry data
  let spans = TelemetryProvider::get_collected_spans(provider)
  let metrics = TelemetryProvider::get_collected_metrics(provider)
  let logs = TelemetryProvider::get_collected_logs(provider)
  
  // Verify correlation
  assert_eq(spans.length(), 1)
  assert_eq(metrics.length(), 2)
  assert_eq(logs.length(), 3)
  
  // Verify trace ID correlation
  let span = spans[0]
  assert_eq(span.trace_id, trace_id)
  assert_eq(span.span_id, span_id)
  
  let correlated_metrics = metrics.filter(fn(m) { 
    m.attributes.contains(("trace_id", trace_id)) && m.attributes.contains(("span_id", span_id))
  })
  assert_eq(correlated_metrics.length(), 2)
  
  let correlated_logs = logs.filter(fn(l) { 
    l.attributes.contains(("trace_id", trace_id)) && l.attributes.contains(("span_id", span_id))
  })
  assert_eq(correlated_logs.length(), 3)
  
  // Create correlation report
  let correlation_report = {
    trace_id: trace_id,
    span_count: spans.length(),
    metric_count: correlated_metrics.length(),
    log_count: correlated_logs.length(),
    status: "error",
    events: span.events.length()
  }
  
  assert_eq(correlation_report.trace_id, trace_id)
  assert_eq(correlation_report.span_count, 1)
  assert_eq(correlation_report.metric_count, 2)
  assert_eq(correlation_report.log_count, 3)
  assert_eq(correlation_report.status, "error")
  assert_eq(correlation_report.events, 3)
}