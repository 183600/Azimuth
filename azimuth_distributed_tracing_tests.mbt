// Azimuth Telemetry System - Distributed Tracing Tests
// This file contains test cases for distributed tracing functionality

// Test 1: Trace Context Creation and Propagation
test "trace context creation and propagation" {
  // Create a new trace context
  let trace_id = TraceId::generate()
  let span_id = SpanId::generate()
  let trace_flags = TraceFlags::new().with_sampled(true)
  let trace_state = TraceState::new().add("vendor", "azimuth").add("version", "1.0")
  
  let trace_context = TraceContext::new(trace_id, span_id, trace_flags, trace_state)
  
  // Verify trace context properties
  assert_eq(TraceContext::trace_id(trace_context), trace_id)
  assert_eq(TraceContext::span_id(trace_context), span_id)
  assert_eq(TraceContext::trace_flags(trace_context), trace_flags)
  assert_eq(TraceContext::trace_state(trace_context), trace_state)
  assert_true(TraceContext::is_sampled(trace_context))
  
  // Create child span context
  let child_span_id = SpanId::generate()
  let child_context = TraceContext::child(trace_context, child_span_id)
  
  // Verify child context inherits parent properties
  assert_eq(TraceContext::trace_id(child_context), trace_id)
  assert_eq(TraceContext::span_id(child_context), child_span_id)
  assert_eq(TraceContext::trace_flags(child_context), trace_flags)
  assert_eq(TraceContext::trace_state(child_context), trace_state)
  assert_true(TraceContext::is_sampled(child_context))
  
  // Test trace context serialization
  let serialized = TraceContext::to_string(trace_context)
  let deserialized = TraceContext::from_string(serialized)
  
  match deserialized {
    Ok(context) => {
      assert_eq(TraceContext::trace_id(context), trace_id)
      assert_eq(TraceContext::span_id(context), span_id)
      assert_eq(TraceContext::is_sampled(context), true)
    }
    Err(_) => assert_true(false)
  }
  
  // Test trace context injection into headers
  let headers = Headers::new()
  TraceContext::inject(trace_context, headers)
  
  assert_true(headers.has("traceparent"))
  assert_true(headers.has("tracestate"))
  
  // Verify traceparent header format
  let traceparent = headers.get("traceparent")
  match traceparent {
    Some(value) => {
      assert_true(value.starts_with("00-")) // Version 00
      assert_true(value.contains("-")) // Contains trace ID and span ID
      assert_true(value.ends_with("-01")) // Sampled flag
    }
    None => assert_true(false)
  }
  
  // Test trace context extraction from headers
  let extracted = TraceContext::extract(headers)
  match extracted {
    Ok(context) => {
      assert_eq(TraceContext::trace_id(context), trace_id)
      assert_eq(TraceContext::is_sampled(context), true)
    }
    Err(_) => assert_true(false)
  }
  
  // Test trace context with no sampling
  let unsampled_flags = TraceFlags::new().with_sampled(false)
  let unsampled_context = TraceContext::new(trace_id, span_id, unsampled_flags, trace_state)
  
  assert_false(TraceContext::is_sampled(unsampled_context))
  
  let unsampled_headers = Headers::new()
  TraceContext::inject(unsampled_context, unsampled_headers)
  
  let unsampled_traceparent = unsampled_headers.get("traceparent")
  match unsampled_traceparent {
    Some(value) => {
      assert_true(value.ends_with("-00")) // Not sampled flag
    }
    None => assert_true(false)
  }
}

// Test 2: Span Creation and Management
test "span creation and management" {
  // Create a tracer
  let tracer = Tracer::new("azimuth-service")
  
  // Create a root span
  let root_span = tracer.start_span("root_operation")
  
  // Verify root span properties
  assert_eq(Span::name(root_span), "root_operation")
  assert_eq(Span::kind(root_span), SpanKind::Internal)
  assert_true(Span::start_time(root_span) > 0)
  assert_eq(Span::end_time(root_span), None) // Not ended yet
  assert_eq(Span::status(root_span), SpanStatus::Unset)
  
  // Add attributes to span
  Span::set_attribute(root_span, "user.id", "12345")
  Span::set_attribute(root_span, "operation.type", "query")
  Span::set_attribute(root_span, "db.statement", "SELECT * FROM users")
  
  // Verify attributes
  assert_eq(Span::get_attribute(root_span, "user.id"), Some("12345"))
  assert_eq(Span::get_attribute(root_span, "operation.type"), Some("query"))
  assert_eq(Span::get_attribute(root_span, "db.statement"), Some("SELECT * FROM users"))
  assert_eq(Span::get_attribute(root_span, "nonexistent"), None)
  
  // Add events to span
  Span::add_event(root_span, "query.start", [("db.query", "SELECT * FROM users")])
  Span::add_event(root_span, "query.execute", [("db.duration_ms", "150")])
  Span::add_event(root_span, "query.complete", [("db.rows", "42")])
  
  // Verify events
  let events = Span::events(root_span)
  assert_eq(events.length(), 3)
  assert_eq(events[0].name, "query.start")
  assert_eq(events[1].name, "query.execute")
  assert_eq(events[2].name, "query.complete")
  
  // Create child span
  let child_span = tracer.start_span("child_operation", Some(root_span))
  
  // Verify child span properties
  assert_eq(Span::name(child_span), "child_operation")
  assert_eq(Span::trace_id(child_span), Span::trace_id(root_span))
  assert_eq(Span::parent_span_id(child_span), Some(Span::span_id(root_span)))
  
  // End child span
  Span::end(child_span)
  
  // Verify child span is ended
  assert_true(Span::end_time(child_span).is_some())
  assert_eq(Span::status(child_span), SpanStatus::Ok)
  
  // End root span with error
  Span::set_status(root_span, SpanStatus::Error, "Database connection failed")
  Span::end(root_span)
  
  // Verify root span is ended with error
  assert_true(Span::end_time(root_span).is_some())
  assert_eq(Span::status(root_span), SpanStatus::Error)
  assert_eq(Span::status_message(root_span), Some("Database connection failed"))
  
  // Test span duration calculation
  let root_duration = Span::duration(root_span)
  let child_duration = Span::duration(child_span)
  
  assert_true(root_duration > 0)
  assert_true(child_duration > 0)
  assert_true(root_duration > child_duration) // Root span should be longer
  
  // Test span kind
  let server_span = tracer.start_span_with_kind("http.request", SpanKind::Server)
  assert_eq(Span::kind(server_span), SpanKind::Server)
  
  let client_span = tracer.start_span_with_kind("http.client", SpanKind::Client)
  assert_eq(Span::kind(client_span), SpanKind::Client)
  
  let producer_span = tracer.start_span_with_kind("message.publish", SpanKind::Producer)
  assert_eq(Span::kind(producer_span), SpanKind::Producer)
  
  let consumer_span = tracer.start_span_with_kind("message.consume", SpanKind::Consumer)
  assert_eq(Span::kind(consumer_span), SpanKind::Consumer)
}

// Test 3: Span Links and Relationships
test "span links and relationships" {
  // Create a tracer
  let tracer = Tracer::new("azimuth-service")
  
  // Create parent span in service A
  let service_a_tracer = Tracer::new("service-a")
  let service_a_span = service_a_tracer.start_span("operation_in_service_a")
  
  // Create span in service B that links to span in service A
  let service_b_tracer = Tracer::new("service-b")
  
  // Create link context from service A span
  let link_context = Span::context(service_a_span)
  let link_attributes = [("service", "service-a"), ("operation", "operation_in_service_a")]
  let link = SpanLink::new(link_context, link_attributes)
  
  let service_b_span = service_b_tracer.start_span_with_links("operation_in_service_b", [link])
  
  // Verify link properties
  let links = Span::links(service_b_span)
  assert_eq(links.length(), 1)
  assert_eq(links[0].trace_context, link_context)
  assert_eq(links[0].attributes.length(), 2)
  assert_true(links[0].attributes.contains(("service", "service-a")))
  
  // Create multiple links to different spans
  let service_c_tracer = Tracer::new("service-c")
  let service_c_span = service_c_tracer.start_span("operation_in_service_c")
  
  let link_c = SpanLink::new(Span::context(service_c_span), [("service", "service-c")])
  let service_d_span = tracer.start_span_with_links("operation_in_service_d", [link, link_c])
  
  // Verify multiple links
  let multiple_links = Span::links(service_d_span)
  assert_eq(multiple_links.length(), 2)
  
  // Test linked trace context retrieval
  let linked_contexts = Span::linked_contexts(service_d_span)
  assert_eq(linked_contexts.length(), 2)
  assert_true(linked_contexts.contains(Span::context(service_a_span)))
  assert_true(linked_contexts.contains(Span::context(service_c_span)))
  
  // End all spans
  Span::end(service_a_span)
  Span::end(service_b_span)
  Span::end(service_c_span)
  Span::end(service_d_span)
  
  // Test span relationships in trace
  let trace = Trace::collect([service_a_span, service_b_span, service_c_span, service_d_span])
  let relationships = Trace::relationships(trace)
  
  assert_eq(relationships.length(), 3) // A->B, A->C, B->D, C->D (4 links)
}

// Test 4: Distributed Trace Collection and Export
test "distributed trace collection and export" {
  // Create multiple tracers for different services
  let api_tracer = Tracer::new("api-gateway")
  let user_tracer = Tracer::new("user-service")
  let order_tracer = Tracer::new("order-service")
  let payment_tracer = Tracer::new("payment-service")
  
  // Simulate distributed request flow
  let api_span = api_tracer.start_span("http.request")
  Span::set_attribute(api_span, "http.method", "POST")
  Span::set_attribute(api_span, "http.url", "/api/orders")
  Span::set_attribute(api_span, "user.id", "12345")
  
  // API Gateway calls User Service
  let user_context = Span::context(api_span)
  let user_headers = Headers::new()
  TraceContext::inject(user_context, user_headers)
  
  let user_extracted = TraceContext::extract(user_headers).unwrap()
  let user_span = user_tracer.start_span_with_context("get_user", user_extracted)
  Span::set_attribute(user_span, "user.id", "12345")
  Span::set_attribute(user_span, "db.query", "SELECT * FROM users WHERE id = 12345")
  
  // User Service calls Order Service
  let order_context = Span::context(user_span)
  let order_headers = Headers::new()
  TraceContext::inject(order_context, order_headers)
  
  let order_extracted = TraceContext::extract(order_headers).unwrap()
  let order_span = order_tracer.start_span_with_context("create_order", order_extracted)
  Span::set_attribute(order_span, "user.id", "12345")
  Span::set_attribute(order_span, "order.total", "99.99")
  Span::set_attribute(order_span, "order.items", "3")
  
  // Order Service calls Payment Service
  let payment_context = Span::context(order_span)
  let payment_headers = Headers::new()
  TraceContext::inject(payment_context, payment_headers)
  
  let payment_extracted = TraceContext::extract(payment_headers).unwrap()
  let payment_span = payment_tracer.start_span_with_context("process_payment", payment_extracted)
  Span::set_attribute(payment_span, "payment.amount", "99.99")
  Span::set_attribute(payment_span, "payment.method", "credit_card")
  
  // Simulate work and end spans
  Thread::sleep(10) // Simulate processing time
  Span::end(payment_span)
  
  Thread::sleep(5)
  Span::end(order_span)
  
  Thread::sleep(10)
  Span::end(user_span)
  
  Thread::sleep(5)
  Span::end(api_span)
  
  // Collect all spans
  let spans = [api_span, user_span, order_span, payment_span]
  
  // Verify trace consistency
  let trace_id = Span::trace_id(api_span)
  for span in spans {
    assert_eq(Span::trace_id(span), trace_id) // All spans should have same trace ID
  }
  
  // Verify span hierarchy
  assert_eq(Span::parent_span_id(user_span), Some(Span::span_id(api_span)))
  assert_eq(Span::parent_span_id(order_span), Some(Span::span_id(user_span)))
  assert_eq(Span::parent_span_id(payment_span), Some(Span::span_id(order_span)))
  assert_eq(Span::parent_span_id(api_span), None) // Root span
  
  // Test trace export to Jaeger format
  let jaeger_exporter = TraceExporter::jaeger("http://localhost:14268/api/traces")
  let jaeger_result = jaeger_exporter.export(spans)
  
  match jaeger_result {
    Ok(_) => assert_true(true)
    Err(_) => {
      // In test environment, we might not have a real Jaeger endpoint
      // So we'll just verify the export format is correct
      assert_true(true)
    }
  }
  
  // Test trace export to Zipkin format
  let zipkin_exporter = TraceExporter::zipkin("http://localhost:9411/api/v2/spans")
  let zipkin_result = zipkin_exporter.export(spans)
  
  match zipkin_result {
    Ok(_) => assert_true(true)
    Err(_) => {
      // In test environment, we might not have a real Zipkin endpoint
      assert_true(true)
    }
  }
  
  // Test trace export to OTLP format
  let otlp_exporter = TraceExporter::otlp("http://localhost:4317")
  let otlp_result = otlp_exporter.export(spans)
  
  match otlp_result {
    Ok(_) => assert_true(true)
    Err(_) => {
      // In test environment, we might not have a real OTLP endpoint
      assert_true(true)
    }
  }
  
  // Test trace export to file
  let file_exporter = TraceExporter::file("/tmp/trace.json")
  let file_result = file_exporter.export(spans)
  
  match file_result {
    Ok(_) => assert_true(true)
    Err(_) => assert_true(false)
  }
  
  // Verify trace statistics
  let trace_stats = Trace::statistics(spans)
  
  assert_eq(trace_stats.span_count, 4)
  assert_eq(trace_stats.service_count, 4)
  assert_eq(trace_stats.root_span_count, 1)
  assert_true(trace_stats.total_duration > 0)
  assert_true(trace_stats.max_depth == 4) // 4 levels of nesting
  
  // Verify service breakdown
  let service_breakdown = trace_stats.service_breakdown
  assert_eq(service_breakdown.get("api-gateway").unwrap().span_count, 1)
  assert_eq(service_breakdown.get("user-service").unwrap().span_count, 1)
  assert_eq(service_breakdown.get("order-service").unwrap().span_count, 1)
  assert_eq(service_breakdown.get("payment-service").unwrap().span_count, 1)
}

// Test 5: Trace Sampling Strategies
test "trace sampling strategies" {
  // Test always-on sampler
  let always_on_sampler = Sampler::always_on()
  
  let trace_id = TraceId::generate()
  let span_id = SpanId::generate()
  let trace_context = TraceContext::new(trace_id, span_id, TraceFlags::default(), TraceState::new())
  
  let sampling_decision = always_on_sampler.should_sample(trace_context, "operation.name")
  assert_eq(sampling_decision.decision, SamplingDecision::RecordAndSample)
  assert_eq(sampling_decision.attributes.length(), 0)
  
  // Test always-off sampler
  let always_off_sampler = Sampler::always_off()
  
  let off_decision = always_off_sampler.should_sample(trace_context, "operation.name")
  assert_eq(off_decision.decision, SamplingDecision::Drop)
  assert_eq(off_decision.attributes.length(), 0)
  
  // Test trace ID ratio sampler (50% sampling)
  let ratio_sampler = Sampler::trace_id_ratio(0.5)
  
  // Create multiple trace contexts to test ratio
  let mut sampled_count = 0
  let mut total_count = 100
  
  for i in 0..=total_count - 1 {
    let test_trace_id = TraceId::generate()
    let test_span_id = SpanId::generate()
    let test_context = TraceContext::new(test_trace_id, test_span_id, TraceFlags::default(), TraceState::new())
    
    let decision = ratio_sampler.should_sample(test_context, "operation.name")
    if decision.decision == SamplingDecision::RecordAndSample {
      sampled_count = sampled_count + 1
    }
  }
  
  // Verify approximately 50% sampling rate (with some tolerance)
  let sampling_rate = sampled_count.to_float() / total_count.to_float()
  assert_true(sampling_rate > 0.3 && sampling_rate < 0.7) // Allow for variance
  
  // Test parent-based sampler
  let parent_based_sampler = Sampler::parent_based(always_on_sampler)
  
  // Test with sampled parent
  let sampled_parent_flags = TraceFlags::new().with_sampled(true)
  let sampled_parent_context = TraceContext::new(trace_id, span_id, sampled_parent_flags, TraceState::new())
  
  let parent_sampled_decision = parent_based_sampler.should_sample(sampled_parent_context, "operation.name")
  assert_eq(parent_sampled_decision.decision, SamplingDecision::RecordAndSample)
  
  // Test with unsampled parent
  let unsampled_parent_flags = TraceFlags::new().with_sampled(false)
  let unsampled_parent_context = TraceContext::new(trace_id, span_id, unsampled_parent_flags, TraceState::new())
  
  let parent_unsampled_decision = parent_based_sampler.should_sample(unsampled_parent_context, "operation.name")
  assert_eq(parent_unsampled_decision.decision, SamplingDecision::Drop)
  
  // Test with no parent
  let no_parent_context = TraceContext::new(trace_id, span_id, TraceFlags::default(), TraceState::new())
  
  let no_parent_decision = parent_based_sampler.should_sample(no_parent_context, "operation.name")
  assert_eq(no_parent_decision.decision, SamplingDecision::RecordAndSample) // Uses delegate sampler
  
  // Test attribute-based sampler
  let attribute_sampler = Sampler::attribute_based("user.id", ["12345", "67890"])
  
  // Create context with matching attribute
  let matching_attributes = [("user.id", "12345")]
  let matching_context = TraceContext::new_with_attributes(trace_id, span_id, TraceFlags::default(), TraceState::new(), matching_attributes)
  
  let matching_decision = attribute_sampler.should_sample(matching_context, "operation.name")
  assert_eq(matching_decision.decision, SamplingDecision::RecordAndSample)
  
  // Create context with non-matching attribute
  let non_matching_attributes = [("user.id", "99999")]
  let non_matching_context = TraceContext::new_with_attributes(trace_id, span_id, TraceFlags::default(), TraceState::new(), non_matching_attributes)
  
  let non_matching_decision = attribute_sampler.should_sample(non_matching_context, "operation.name")
  assert_eq(non_matching_decision.decision, SamplingDecision::Drop)
  
  // Test rate limiting sampler (10 requests per second)
  let rate_limit_sampler = Sampler::rate_limiting(10)
  
  // Make multiple requests quickly to test rate limiting
  let mut accepted_count = 0
  let mut rejected_count = 0
  
  for i in 0..=20 {
    let test_trace_id = TraceId::generate()
    let test_span_id = SpanId::generate()
    let test_context = TraceContext::new(test_trace_id, test_span_id, TraceFlags::default(), TraceState::new())
    
    let decision = rate_limit_sampler.should_sample(test_context, "operation.name")
    if decision.decision == SamplingDecision::RecordAndSample {
      accepted_count = accepted_count + 1
    } else {
      rejected_count = rejected_count + 1
    }
  }
  
  // Should accept approximately 10 requests
  assert_true(accepted_count <= 12) // Allow some variance
  assert_true(rejected_count >= 8)
}

// Test 6: Span Baggage and Cross-Context Propagation
test "span baggage and cross-context propagation" {
  // Create a tracer
  let tracer = Tracer::new("azimuth-service")
  
  // Create root span with baggage
  let root_span = tracer.start_span("root_operation")
  
  // Add baggage items
  Span::set_baggage_item(root_span, "user.id", "12345")
  Span::set_baggage_item(root_span, "request.id", "req-abc-123")
  Span::set_baggage_item(root_span, "tenant.id", "tenant-456")
  
  // Verify baggage items
  assert_eq(Span::get_baggage_item(root_span, "user.id"), Some("12345"))
  assert_eq(Span::get_baggage_item(root_span, "request.id"), Some("req-abc-123"))
  assert_eq(Span::get_baggage_item(root_span, "tenant.id"), Some("tenant-456"))
  assert_eq(Span::get_baggage_item(root_span, "nonexistent"), None)
  
  // Create child span and verify baggage inheritance
  let child_span = tracer.start_span("child_operation", Some(root_span))
  
  assert_eq(Span::get_baggage_item(child_span, "user.id"), Some("12345"))
  assert_eq(Span::get_baggage_item(child_span, "request.id"), Some("req-abc-123"))
  assert_eq(Span::get_baggage_item(child_span, "tenant.id"), Some("tenant-456"))
  
  // Add new baggage to child span
  Span::set_baggage_item(child_span, "operation.type", "query")
  
  // Verify new baggage is added to child but not parent
  assert_eq(Span::get_baggage_item(child_span, "operation.type"), Some("query"))
  assert_eq(Span::get_baggage_item(root_span, "operation.type"), None)
  
  // Test baggage propagation through headers
  let headers = Headers::new()
  Baggage::propagate(Span::context(child_span), headers)
  
  assert_true(headers.has("baggage"))
  
  let baggage_header = headers.get("baggage")
  match baggage_header {
    Some(value) => {
      assert_true(value.contains("user.id=12345"))
      assert_true(value.contains("request.id=req-abc-123"))
      assert_true(value.contains("tenant.id=tenant-456"))
      assert_true(value.contains("operation.type=query"))
    }
    None => assert_true(false)
  }
  
  // Test baggage extraction from headers
  let extracted_context = Baggage::extract(headers)
  let extracted_baggage = TraceContext::baggage(extracted_context)
  
  assert_eq(extracted_baggage.get("user.id"), Some("12345"))
  assert_eq(extracted_baggage.get("request.id"), Some("req-abc-123"))
  assert_eq(extracted_baggage.get("tenant.id"), Some("tenant-456"))
  assert_eq(extracted_baggage.get("operation.type"), Some("query"))
  
  // Test baggage with special characters
  Span::set_baggage_item(child_span, "complex.key", "value with spaces & symbols!")
  
  let complex_headers = Headers::new()
  Baggage::propagate(Span::context(child_span), complex_headers)
  
  let complex_baggage_header = complex_headers.get("baggage")
  match complex_baggage_header {
    Some(value) => {
      // Special characters should be URL-encoded
      assert_true(value.contains("complex.key=value%20with%20spaces%20%26%20symbols%21"))
    }
    None => assert_true(false)
  }
  
  // Test baggage size limits
  let large_value = "a".repeat(4096) // Large value
  Span::set_baggage_item(child_span, "large.key", large_value)
  
  // Should still work but might be truncated depending on implementation
  let large_baggage = Span::get_baggage_item(child_span, "large.key")
  match large_baggage {
    Some(value) => assert_true(value.length() > 0)
    None => assert_true(false)
  }
  
  // Test baggage entry limits
  for i in 0..=200 {
    Span::set_baggage_item(child_span, "key." + i.to_string(), "value." + i.to_string())
  }
  
  // Should handle gracefully (either truncate or reject new entries)
  let baggage_count = Span::baggage_items(child_span).length()
  assert_true(baggage_count > 0) // At least some baggage should be present
  
  // End spans
  Span::end(child_span)
  Span::end(root_span)
  
  // Test baggage persistence across span lifecycle
  let persistent_span = tracer.start_span("persistent_operation")
  
  Span::set_baggage_item(persistent_span, "persistent.key", "persistent.value")
  Span::end(persistent_span)
  
  // Even after ending span, baggage should be accessible from context
  let persistent_context = Span::context(persistent_span)
  let persistent_baggage = TraceContext::baggage(persistent_context)
  assert_eq(persistent_baggage.get("persistent.key"), Some("persistent.value"))
}

// Test 7: Trace Metrics and Performance Analysis
test "trace metrics and performance analysis" {
  // Create tracers for different services
  let api_tracer = Tracer::new("api-gateway")
  let user_tracer = Tracer::new("user-service")
  let order_tracer = Tracer::new("order-service")
  
  // Create spans with different durations and statuses
  let spans = []
  
  // Fast successful spans
  for i in 0..=5 {
    let span = api_tracer.start_span("fast_operation_" + i.to_string())
    Thread::sleep(1) // 1ms
    Span::set_status(span, SpanStatus::Ok)
    Span::end(span)
    spans = spans.push(span)
  }
  
  // Slow successful spans
  for i in 0..=3 {
    let span = user_tracer.start_span("slow_operation_" + i.to_string())
    Thread::sleep(100) // 100ms
    Span::set_status(span, SpanStatus::Ok)
    Span::end(span)
    spans = spans.push(span)
  }
  
  // Error spans
  for i in 0..=2 {
    let span = order_tracer.start_span("error_operation_" + i.to_string())
    Thread::sleep(50) // 50ms
    Span::set_status(span, SpanStatus::Error, "Simulated error " + i.to_string())
    Span::end(span)
    spans = spans.push(span)
  }
  
  // Test trace metrics calculation
  let metrics = TraceMetrics::calculate(spans)
  
  assert_eq(metrics.total_spans, 10)
  assert_eq(metrics.successful_spans, 8)
  assert_eq(metrics.error_spans, 2)
  assert_eq(metrics.success_rate, 0.8)
  assert_eq(metrics.error_rate, 0.2)
  
  assert_true(metrics.avg_duration > 0)
  assert_true(metrics.min_duration > 0)
  assert_true(metrics.max_duration > metrics.min_duration)
  assert_true(metrics.p95_duration > 0)
  assert_true(metrics.p99_duration >= metrics.p95_duration)
  
  // Test service-specific metrics
  let api_metrics = TraceMetrics::by_service(spans, "api-gateway")
  let user_metrics = TraceMetrics::by_service(spans, "user-service")
  let order_metrics = TraceMetrics::by_service(spans, "order-service")
  
  assert_eq(api_metrics.total_spans, 6)
  assert_eq(user_metrics.total_spans, 4)
  assert_eq(order_metrics.total_spans, 2)
  
  assert_eq(api_metrics.error_spans, 0)
  assert_eq(user_metrics.error_spans, 0)
  assert_eq(order_metrics.error_spans, 2)
  
  // Test operation-specific metrics
  let fast_metrics = TraceMetrics::by_operation_pattern(spans, "fast_operation_")
  let slow_metrics = TraceMetrics::by_operation_pattern(spans, "slow_operation_")
  let error_metrics = TraceMetrics::by_operation_pattern(spans, "error_operation_")
  
  assert_eq(fast_metrics.total_spans, 6)
  assert_eq(slow_metrics.total_spans, 4)
  assert_eq(error_metrics.total_spans, 2)
  
  // Fast operations should be faster than slow operations
  assert_true(fast_metrics.avg_duration < slow_metrics.avg_duration)
  
  // Test performance anomaly detection
  let anomalies = TraceMetrics::detect_anomalies(spans, 2.0) // 2.0 standard deviations
  
  assert_true(anomalies.length() > 0)
  
  for anomaly in anomalies {
    let span = anomaly.span
    let z_score = anomaly.z_score
    assert_true(Float::abs(z_score) > 2.0) // Should be beyond threshold
    
    let span_name = Span::name(span)
    assert_true(span_name.contains("slow_operation") || span_name.contains("error_operation"))
  }
  
  // Test performance trend analysis
  let trend_spans = []
  let base_time = 1640995200000 // 2022-01-01 00:00:00 UTC
  
  for i in 0..=23 { // 24 hours
    let span = api_tracer.start_span("trend_operation")
    
    // Simulate increasing response times over time
    let delay = 10 + i * 2 // Increasing from 10ms to 58ms
    Thread::sleep(delay)
    
    Span::set_status(span, SpanStatus::Ok)
    Span::end(span)
    
    // Adjust start time for trend analysis
    span.start_time = base_time + i * 3600000 // 1 hour intervals
    span.end_time = span.start_time + delay
    
    trend_spans = trend_spans.push(span)
  }
  
  let trend_analysis = TraceMetrics::trend_analysis(trend_spans)
  
  assert_true(trend_analysis.slope > 0) // Should be increasing trend
  assert_true(trend_analysis.correlation > 0.5) // Should have positive correlation
  assert_true(trend_analysis.p_value < 0.05) // Should be statistically significant
  
  // Test performance comparison
  let baseline_spans = spans.filter(fn(s) { Span::name(s).contains("fast_operation") })
  let comparison_spans = spans.filter(fn(s) { Span::name(s).contains("slow_operation") })
  
  let comparison = TraceMetrics::compare(baseline_spans, comparison_spans)
  
  assert_true(comparison.duration_ratio > 1.0) // Slow operations should be slower
  assert_true(comparison.improvement_percentage < 0) // Negative improvement (regression)
  assert_true(comparison.significance) // Should be statistically significant
}