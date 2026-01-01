// Cross-Service Tracing Consistency Tests
// This file contains test cases for cross-service tracing consistency and propagation

test "cross-service trace context propagation" {
  // Test trace context propagation across multiple services
  let root_trace_id = "trace-abc123def456"
  let root_span_id = "span-789012345678"
  
  // Service A: Create root span
  let service_a_ctx = azimuth::SpanContext::new(root_trace_id, root_span_id, true, "service=A")
  let service_a_span = azimuth::Span::new("service-a-operation", azimuth::Server, service_a_ctx)
  
  // Add baggage for cross-service communication
  let baggage = azimuth::Baggage::new()
  let baggage_with_user = azimuth::Baggage::set_entry(baggage, "user.id", "user-12345")
  let baggage_with_session = azimuth::Baggage::set_entry(baggage_with_user, "session.id", "session-67890")
  
  // Extract headers for propagation
  let headers = azimuth::Propagator::extract_headers(service_a_ctx, baggage_with_session)
  assert_true(headers.length > 0)
  
  // Service B: Inject context from headers
  let (service_b_ctx, service_b_baggage) = azimuth::Propagator::inject_context(headers)
  assert_eq(azimuth::SpanContext::trace_id(service_b_ctx), root_trace_id)
  assert_eq(azimuth::SpanContext::trace_flags(service_b_ctx), azimuth::SpanContext::trace_flags(service_a_ctx))
  
  // Service B creates child span
  let service_b_span_id = "span-b-child-123"
  let service_b_child_ctx = azimuth::SpanContext::with_parent(service_b_ctx, service_b_span_id)
  let service_b_span = azimuth::Span::new("service-b-operation", azimuth::Internal, service_b_child_ctx)
  
  // Verify baggage propagation
  let user_id = azimuth::Baggage::get_entry(service_b_baggage, "user.id")
  let session_id = azimuth::Baggage::get_entry(service_b_baggage, "session.id")
  assert_eq(user_id, Some("user-12345"))
  assert_eq(session_id, Some("session-67890"))
  
  // Service C: Continue the trace
  let service_c_headers = azimuth::Propagator::extract_headers(service_b_child_ctx, service_b_baggage)
  let (service_c_ctx, service_c_baggage) = azimuth::Propagator::inject_context(service_c_headers)
  
  let service_c_span = azimuth::Span::new("service-c-operation", azimuth::Client, service_c_ctx)
  
  // Verify trace consistency across all services
  assert_eq(azimuth::SpanContext::trace_id(azimuth::Span::context(service_a_span)), root_trace_id)
  assert_eq(azimuth::SpanContext::trace_id(azimuth::Span::context(service_b_span)), root_trace_id)
  assert_eq(azimuth::SpanContext::trace_id(azimuth::Span::context(service_c_span)), root_trace_id)
  
  // Verify parent-child relationships
  assert_eq(azimuth::SpanContext::parent_span_id(service_b_child_ctx), root_span_id)
}

test "distributed tracing with sampling decisions" {
  // Test distributed tracing with consistent sampling decisions
  let trace_id = "trace-sampling-test-123"
  
  // Create sampler with specific sampling rate
  let sampler = azimuth::Sampler::trace_id_ratio_based(0.5)  // 50% sampling rate
  let sampling_decision = azimuth::Sampler::should_sample(sampler, trace_id, "operation-name", azimuth::Internal)
  
  // Root service makes sampling decision
  let root_ctx = azimuth::SpanContext::with_sampling(trace_id, "span-root", sampling_decision.is_sampled)
  let root_span = azimuth::Span::new("root-operation", azimuth::Server, root_ctx)
  
  // Propagate sampling decision
  let headers = azimuth::Propagator::extract_headers(root_ctx, azimuth::Baggage::new())
  let (propagated_ctx, _) = azimuth::Propagator::inject_context(headers)
  
  // Child services respect sampling decision
  let child_span1 = azimuth::Span::new("child-operation-1", azimuth::Internal, propagated_ctx)
  let child_span2 = azimuth::Span::new("child-operation-2", azimuth::Client, propagated_ctx)
  
  // Verify sampling consistency
  assert_eq(azimuth::SpanContext::is_sampled(azimuth::Span::context(root_span)), sampling_decision.is_sampled)
  assert_eq(azimuth::SpanContext::is_sampled(azimuth::Span::context(child_span1)), sampling_decision.is_sampled)
  assert_eq(azimuth::SpanContext::is_sampled(azimuth::Span::context(child_span2)), sampling_decision.is_sampled)
  
  // Verify trace flags consistency
  let root_flags = azimuth::SpanContext::trace_flags(root_ctx)
  let child_flags = azimuth::SpanContext::trace_flags(propagated_ctx)
  assert_eq(root_flags, child_flags)
}

test "cross-service metric correlation" {
  // Test metric correlation across services with trace context
  let trace_id = "trace-metrics-correlation-456"
  let service_a_ctx = azimuth::SpanContext::new(trace_id, "span-a-123", true, "")
  
  // Service A: Create metrics with trace context
  let provider_a = azimuth::MeterProvider::default()
  let meter_a = azimuth::MeterProvider::get_meter(provider_a, "service-a")
  let counter_a = azimuth::Meter::create_counter(meter_a, "operation.count", "Operation count", "1")
  
  azimuth::Counter::add(counter_a, 1, [
    ("service", azimuth::StringValue("service-a")),
    ("trace.id", azimuth::StringValue(trace_id)),
    ("operation", azimuth::StringValue("process-data"))
  ])
  
  // Service B: Continue with correlated metrics
  let service_b_ctx = azimuth::SpanContext::with_parent(service_a_ctx, "span-b-456")
  let provider_b = azimuth::MeterProvider::default()
  let meter_b = azimuth::MeterProvider::get_meter(provider_b, "service-b")
  let counter_b = azimuth::Meter::create_counter(meter_b, "operation.count", "Operation count", "1")
  
  azimuth::Counter::add(counter_b, 1, [
    ("service", azimuth::StringValue("service-b")),
    ("trace.id", azimuth::StringValue(trace_id)),
    ("operation", azimuth::StringValue("transform-data"))
  ])
  
  // Service C: Complete the operation chain
  let service_c_ctx = azimuth::SpanContext::with_parent(service_b_ctx, "span-c-789")
  let provider_c = azimuth::MeterProvider::default()
  let meter_c = azimuth::MeterProvider::get_meter(provider_c, "service-c")
  let counter_c = azimuth::Meter::create_counter(meter_c, "operation.count", "Operation count", "1")
  
  azimuth::Counter::add(counter_c, 1, [
    ("service", azimuth::StringValue("service-c")),
    ("trace.id", azimuth::StringValue(trace_id)),
    ("operation", azimuth::StringValue("store-data"))
  ])
  
  // Collect and correlate metrics
  let metrics_a = azimuth::Meter::collect_metrics(meter_a)
  let metrics_b = azimuth::Meter::collect_metrics(meter_b)
  let metrics_c = azimuth::Meter::collect_metrics(meter_c)
  
  // Verify trace correlation in metrics
  let trace_metrics_a = azimuth::MetricData::filter_by_attribute(metrics_a, "trace.id", trace_id)
  let trace_metrics_b = azimuth::MetricData::filter_by_attribute(metrics_b, "trace.id", trace_id)
  let trace_metrics_c = azimuth::MetricData::filter_by_attribute(metrics_c, "trace.id", trace_id)
  
  assert_eq(trace_metrics_a.length, 1)
  assert_eq(trace_metrics_b.length, 1)
  assert_eq(trace_metrics_c.length, 1)
  
  // Verify service differentiation
  let service_a_metric = trace_metrics_a[0]
  let service_b_metric = trace_metrics_b[0]
  let service_c_metric = trace_metrics_c[0]
  
  assert_eq(azimuth::MetricData::get_attribute(service_a_metric, "service"), Some(azimuth::StringValue("service-a")))
  assert_eq(azimuth::MetricData::get_attribute(service_b_metric, "service"), Some(azimuth::StringValue("service-b")))
  assert_eq(azimuth::MetricData::get_attribute(service_c_metric, "service"), Some(azimuth::StringValue("service-c")))
}