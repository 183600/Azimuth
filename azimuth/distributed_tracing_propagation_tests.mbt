// Distributed Tracing Propagation Tests
// This file contains comprehensive tests for distributed tracing propagation scenarios

test "trace context propagation through http headers" {
  // Test trace context propagation through HTTP headers
  let trace_id = "0af7651916cd43dd8448eb211c80319c"
  let span_id = "b7ad6b7169203331"
  let span_ctx = SpanContext::new(trace_id, span_id, true, "key1=value1")
  
  // Create HTTP carrier with trace context
  let carrier = TextMapCarrier::new()
  TextMapCarrier::set(carrier, "traceparent", "00-0af7651916cd43dd8448eb211c80319c-b7ad6b7169203331-01")
  TextMapCarrier::set(carrier, "tracestate", "key1=value1")
  
  // Simulate HTTP request
  let headers = [("traceparent", "00-0af7651916cd43dd8448eb211c80319c-b7ad6b7169203331-01")]
  let request = HttpRequest::new("GET", "https://api.example.com/data", headers)
  
  // Extract trace context from HTTP request
  let propagator = CompositePropagator::new([W3CTraceContextPropagator::new()])
  let ctx = CompositePropagator::extract(propagator, carrier)
  
  // Verify trace context propagation
  assert_eq(HttpRequest::http_method(request), "GET")
  assert_eq(HttpRequest::url(request), "https://api.example.com/data")
  assert_true(ctx != Context::root())
}

test "span hierarchy relationship in distributed systems" {
  // Test span hierarchy relationships in distributed systems
  let root_trace_id = "1234567890abcdef1234567890abcdef"
  let root_span_id = "1111111111111111"
  let root_ctx = SpanContext::new(root_trace_id, root_span_id, true, "")
  
  let provider = TracerProvider::default()
  let tracer = TracerProvider::get_tracer(provider, "service-a")
  
  // Create root span
  let root_span = Tracer::start_span(tracer, "root-operation")
  assert_eq(Span::name(root_span), "root-operation")
  assert_eq(Span::kind(root_span), Internal)
  assert_true(Span::is_recording(root_span))
  
  // Create child span for service B
  let child_trace_id = SpanContext::trace_id(root_ctx)
  let child_span_id = "2222222222222222"
  let child_ctx = SpanContext::new(child_trace_id, child_span_id, true, "")
  let child_span = Span::new("service-b-operation", Client, child_ctx)
  
  // Create grandchild span for service C
  let grandchild_span_id = "3333333333333333"
  let grandchild_ctx = SpanContext::new(child_trace_id, grandchild_span_id, true, "")
  let grandchild_span = Span::new("service-c-operation", Server, grandchild_ctx)
  
  // Verify span hierarchy
  assert_eq(SpanContext::trace_id(root_ctx), SpanContext::trace_id(child_ctx))
  assert_eq(SpanContext::trace_id(child_ctx), SpanContext::trace_id(grandchild_ctx))
  assert_true(SpanContext::is_valid(root_ctx))
  assert_true(SpanContext::is_valid(child_ctx))
  assert_true(SpanContext::is_valid(grandchild_ctx))
}

test "cross-service trace correlation with baggage" {
  // Test cross-service trace correlation with baggage
  let trace_id = "abcdef1234567890abcdef1234567890"
  let span_id = "aaaabbbbccccdddd"
  let span_ctx = SpanContext::new(trace_id, span_id, true, "")
  
  // Initialize baggage with correlation data
  let baggage = Baggage::new()
  let baggage_with_user = Baggage::set_entry(baggage, "user.id", "user-12345")
  let baggage_with_session = Baggage::set_entry(baggage_with_user, "session.id", "session-67890")
  let baggage_with_request = Baggage::set_entry(baggage_with_session, "request.id", "req-abc123")
  
  // Simulate service A to service B propagation
  let service_a_baggage = Baggage::set_entry(baggage_with_request, "service.a.timestamp", "1640995200")
  let service_b_user_id = Baggage::get_entry(service_a_baggage, "user.id")
  let service_b_session_id = Baggage::get_entry(service_a_baggage, "session.id")
  
  // Simulate service B to service C propagation
  let service_b_baggage = Baggage::set_entry(service_a_baggage, "service.b.duration", "150ms")
  let service_c_user_id = Baggage::get_entry(service_b_baggage, "user.id")
  let service_c_request_id = Baggage::get_entry(service_b_baggage, "request.id")
  let service_c_a_timestamp = Baggage::get_entry(service_b_baggage, "service.a.timestamp")
  
  // Verify correlation integrity
  assert_eq(service_b_user_id, Some("user-12345"))
  assert_eq(service_b_session_id, Some("session-67890"))
  assert_eq(service_c_user_id, Some("user-12345"))
  assert_eq(service_c_request_id, Some("req-abc123"))
  assert_eq(service_c_a_timestamp, Some("1640995200"))
}

test "trace context sampling decision propagation" {
  // Test that sampling decisions are properly propagated
  let sampled_trace_id = "11112222333344445555666677778888"
  let sampled_span_id = "aaaabbbbccccdddd"
  let sampled_ctx = SpanContext::new(sampled_trace_id, sampled_span_id, true, "")
  
  let not_sampled_trace_id = "88887777666655554444333322221111"
  let not_sampled_span_id = "ddddccccbbbbaaaa"
  let not_sampled_ctx = SpanContext::new(not_sampled_trace_id, not_sampled_span_id, false, "")
  
  // Create carriers for both sampled and non-sampled traces
  let sampled_carrier = TextMapCarrier::new()
  TextMapCarrier::set(sampled_carrier, "traceparent", "00-11112222333344445555666677778888-aaaabbbbccccdddd-01")
  
  let not_sampled_carrier = TextMapCarrier::new()
  TextMapCarrier::set(not_sampled_carrier, "traceparent", "00-88887777666655554444333322221111-ddddccccbbbbaaaa-00")
  
  // Extract contexts
  let propagator = CompositePropagator::new([W3CTraceContextPropagator::new()])
  let sampled_extracted = CompositePropagator::extract(propagator, sampled_carrier)
  let not_sampled_extracted = CompositePropagator::extract(propagator, not_sampled_carrier)
  
  // Verify sampling decisions
  assert_true(SpanContext::is_sampled(sampled_ctx))
  assert_false(SpanContext::is_sampled(not_sampled_ctx))
  assert_true(sampled_extracted != Context::root())
  assert_true(not_sampled_extracted != Context::root())
}

test "async operation trace context preservation" {
  // Test trace context preservation in async operations
  let parent_trace_id = "aaaaBBBBccccDDDDeeeeFFFFggggHHHH"
  let parent_span_id = "1111222233334444"
  let parent_ctx = SpanContext::new(parent_trace_id, parent_span_id, true, "key1=value1")
  
  // Simulate async operation start
  let async_start_carrier = TextMapCarrier::new()
  TextMapCarrier::set(async_start_carrier, "traceparent", "00-aaaaBBBBccccDDDDeeeeFFFFggggHHHH-1111222233334444-01")
  TextMapCarrier::set(async_start_carrier, "tracestate", "key1=value1")
  
  // Simulate async operation context preservation
  let propagator = CompositePropagator::new([W3CTraceContextPropagator::new()])
  let async_ctx = CompositePropagator::extract(propagator, async_start_carrier)
  
  // Create child span for async operation
  let async_span_id = "5555666677778888"
  let async_child_ctx = SpanContext::new(parent_trace_id, async_span_id, true, "key1=value1")
  let async_span = Span::new("async-operation", Internal, async_child_ctx)
  
  // Simulate async operation completion
  let async_completion_carrier = TextMapCarrier::new()
  TextMapCarrier::set(async_completion_carrier, "traceparent", "00-aaaaBBBBccccDDDDeeeeFFFFggggHHHH-5555666677778888-01")
  let completion_ctx = CompositePropagator::extract(propagator, async_completion_carrier)
  
  // Verify context preservation
  assert_eq(SpanContext::trace_id(parent_ctx), SpanContext::trace_id(async_child_ctx))
  assert_eq(Span::name(async_span), "async-operation")
  assert_true(completion_ctx != Context::root())
}

test "microservice chain trace context flow" {
  // Test trace context flow through a microservice chain
  let gateway_trace_id = "11111111222222223333333344444444"
  let gateway_span_id = "aaaabbbbccccdddd"
  let gateway_ctx = SpanContext::new(gateway_trace_id, gateway_span_id, true, "")
  
  // Service A (API Gateway)
  let service_a_carrier = TextMapCarrier::new()
  TextMapCarrier::set(service_a_carrier, "traceparent", "00-11111111222222223333333344444444-aaaabbbbccccdddd-01")
  
  // Service B (Auth Service)
  let service_b_span_id = "eeeeffffgggghhhh"
  let service_b_ctx = SpanContext::new(gateway_trace_id, service_b_span_id, true, "")
  let service_b_carrier = TextMapCarrier::new()
  TextMapCarrier::set(service_b_carrier, "traceparent", "00-11111111222222223333333344444444-eeeeffffgggghhhh-01")
  TextMapCarrier::set(service_b_carrier, "user.id", "user-123")
  
  // Service C (Business Logic)
  let service_c_span_id = "iiiijjjjkkkkllll"
  let service_c_ctx = SpanContext::new(gateway_trace_id, service_c_span_id, true, "")
  let service_c_carrier = TextMapCarrier::new()
  TextMapCarrier::set(service_c_carrier, "traceparent", "00-11111111222222223333333344444444-iiiijjjjkkkkllll-01")
  TextMapCarrier::set(service_c_carrier, "user.id", "user-123")
  TextMapCarrier::set(service_c_carrier, "auth.token", "token-abc")
  
  // Service D (Database)
  let service_d_span_id = "mmmmnnnnoooopppp"
  let service_d_ctx = SpanContext::new(gateway_trace_id, service_d_span_id, true, "")
  let service_d_carrier = TextMapCarrier::new()
  TextMapCarrier::set(service_d_carrier, "traceparent", "00-11111111222222223333333344444444-mmmmnnnnoooopppp-01")
  TextMapCarrier::set(service_d_carrier, "user.id", "user-123")
  TextMapCarrier::set(service_d_carrier, "auth.token", "token-abc")
  TextMapCarrier::set(service_d_carrier, "db.query", "SELECT * FROM users")
  
  // Verify trace consistency across the chain
  let propagator = CompositePropagator::new([W3CTraceContextPropagator::new()])
  let a_ctx = CompositePropagator::extract(propagator, service_a_carrier)
  let b_ctx = CompositePropagator::extract(propagator, service_b_carrier)
  let c_ctx = CompositePropagator::extract(propagator, service_c_carrier)
  let d_ctx = CompositePropagator::extract(propagator, service_d_carrier)
  
  // Verify all services have the same trace ID
  assert_eq(SpanContext::trace_id(gateway_ctx), gateway_trace_id)
  assert_eq(SpanContext::trace_id(service_b_ctx), gateway_trace_id)
  assert_eq(SpanContext::trace_id(service_c_ctx), gateway_trace_id)
  assert_eq(SpanContext::trace_id(service_d_ctx), gateway_trace_id)
  
  // Verify context propagation
  assert_true(a_ctx != Context::root())
  assert_true(b_ctx != Context::root())
  assert_true(c_ctx != Context::root())
  assert_true(d_ctx != Context::root())
  
  // Verify baggage propagation
  let b_user_id = TextMapCarrier::get(service_b_carrier, "user.id")
  let c_user_id = TextMapCarrier::get(service_c_carrier, "user.id")
  let d_user_id = TextMapCarrier::get(service_d_carrier, "user.id")
  let d_query = TextMapCarrier::get(service_d_carrier, "db.query")
  
  assert_eq(b_user_id, Some("user-123"))
  assert_eq(c_user_id, Some("user-123"))
  assert_eq(d_user_id, Some("user-123"))
  assert_eq(d_query, Some("SELECT * FROM users"))
}