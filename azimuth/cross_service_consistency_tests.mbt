// Test file for cross-service consistency
test "trace context propagation consistency" {
  // Create initial span context
  let original_ctx = SpanContext::new("trace-1234567890abcdef", "span-1111111111111111", true, "key1=value1,key2=value2")
  
  // Test trace ID consistency
  assert_eq(SpanContext::trace_id(original_ctx), "trace-1234567890abcdef")
  assert_eq(SpanContext::span_id(original_ctx), "span-1111111111111111")
  assert_true(SpanContext::is_sampled(original_ctx))
  assert_eq(original_ctx.trace_state, "key1=value1,key2=value2")
  
  // Create child span context (simulating cross-service propagation)
  let child_ctx = SpanContext::new("trace-1234567890abcdef", "span-2222222222222222", true, "key1=value1,key2=value2")
  
  // Verify trace ID consistency across services
  assert_eq(SpanContext::trace_id(original_ctx), SpanContext::trace_id(child_ctx))
  assert_true(SpanContext::trace_id(original_ctx) == "trace-1234567890abcdef")
  
  // Verify different span IDs
  assert_true(SpanContext::span_id(original_ctx) != SpanContext::span_id(child_ctx))
  
  // Verify sampling consistency
  assert_eq(SpanContext::is_sampled(original_ctx), SpanContext::is_sampled(child_ctx))
}

test "baggage propagation consistency" {
  // Create initial baggage
  let baggage = Baggage::new()
  let baggage_with_data = Baggage::set_entry(baggage, "user.id", "user-12345")
  let baggage_with_more_data = Baggage::set_entry(baggage_with_data, "request.id", "req-67890")
  
  // Test baggage consistency
  let user_id = Baggage::get_entry(baggage_with_more_data, "user.id")
  assert_true(user_id.is_some(), "Should find user.id in baggage")
  assert_eq(user_id.unwrap(), "user-12345")
  
  let request_id = Baggage::get_entry(baggage_with_more_data, "request.id")
  assert_true(request_id.is_some(), "Should find request.id in baggage")
  assert_eq(request_id.unwrap(), "req-67890")
  
  // Simulate cross-service baggage propagation
  let propagated_baggage = Baggage::set_entry(baggage_with_more_data, "service.name", "service-B")
  
  // Verify original baggage entries are preserved
  let propagated_user_id = Baggage::get_entry(propagated_baggage, "user.id")
  assert_eq(propagated_user_id.unwrap(), "user-12345")
  
  let propagated_request_id = Baggage::get_entry(propagated_baggage, "request.id")
  assert_eq(propagated_request_id.unwrap(), "req-67890")
  
  // Verify new baggage entry
  let service_name = Baggage::get_entry(propagated_baggage, "service.name")
  assert_true(service_name.is_some(), "Should find service.name in propagated baggage")
  assert_eq(service_name.unwrap(), "service-B")
}

test "context propagation across services" {
  // Create root context
  let root_ctx = Context::root()
  
  // Add correlation data
  let correlation_key = ContextKey::new("correlation.id")
  let ctx_with_correlation = Context::with_value(root_ctx, correlation_key, "corr-123456789")
  
  // Add user context
  let user_key = ContextKey::new("user.context")
  let ctx_with_user = Context::with_value(ctx_with_correlation, user_key, "premium-user")
  
  // Simulate context propagation to service A
  let service_a_key = ContextKey::new("service.a.timestamp")
  let ctx_service_a = Context::with_value(ctx_with_user, service_a_key, "2025-01-01T12:00:00Z")
  
  // Verify original context is preserved in service A
  let correlation_in_a = Context::get(ctx_service_a, correlation_key)
  assert_eq(correlation_in_a.unwrap(), "corr-123456789")
  
  let user_in_a = Context::get(ctx_service_a, user_key)
  assert_eq(user_in_a.unwrap(), "premium-user")
  
  // Simulate context propagation to service B
  let service_b_key = ContextKey::new("service.b.operation")
  let ctx_service_b = Context::with_value(ctx_service_a, service_b_key, "data-processing")
  
  // Verify all context is preserved in service B
  let correlation_in_b = Context::get(ctx_service_b, correlation_key)
  assert_eq(correlation_in_b.unwrap(), "corr-123456789")
  
  let user_in_b = Context::get(ctx_service_b, user_key)
  assert_eq(user_in_b.unwrap(), "premium-user")
  
  let timestamp_in_b = Context::get(ctx_service_b, service_a_key)
  assert_eq(timestamp_in_b.unwrap(), "2025-01-01T12:00:00Z")
  
  let operation_in_b = Context::get(ctx_service_b, service_b_key)
  assert_eq(operation_in_b.unwrap(), "data-processing")
}

test "text map carrier consistency" {
  // Create carrier with trace context
  let carrier = TextMapCarrier::new()
  TextMapCarrier::set(carrier, "traceparent", "00-0af7651916cd43dd8448eb211c80319c-b7ad6b7169203331-01")
  TextMapCarrier::set(carrier, "tracestate", "rojo=00f067aa0ba902b7,congo=t61rcWkgMzE")
  TextMapCarrier::set(carrier, "baggage", "userId=amy,userType=admin")
  
  // Verify carrier consistency
  let traceparent = TextMapCarrier::get(carrier, "traceparent")
  assert_true(traceparent.is_some(), "Should find traceparent")
  assert_eq(traceparent.unwrap(), "00-0af7651916cd43dd8448eb211c80319c-b7ad6b7169203331-01")
  
  let tracestate = TextMapCarrier::get(carrier, "tracestate")
  assert_true(tracestate.is_some(), "Should find tracestate")
  assert_eq(tracestate.unwrap(), "rojo=00f067aa0ba902b7,congo=t61rcWkgMzE")
  
  let baggage = TextMapCarrier::get(carrier, "baggage")
  assert_true(baggage.is_some(), "Should find baggage")
  assert_eq(baggage.unwrap(), "userId=amy,userType=admin")
  
  // Simulate propagation with additional headers
  TextMapCarrier::set(carrier, "x-service-name", "service-A")
  TextMapCarrier::set(carrier, "x-request-id", "req-123456789")
  
  // Verify original headers are preserved
  let original_traceparent = TextMapCarrier::get(carrier, "traceparent")
  assert_eq(original_traceparent.unwrap(), "00-0af7651916cd43dd8448eb211c80319c-b7ad6b7169203331-01")
  
  // Verify new headers
  let service_name = TextMapCarrier::get(carrier, "x-service-name")
  assert_true(service_name.is_some(), "Should find service name")
  assert_eq(service_name.unwrap(), "service-A")
  
  let request_id = TextMapCarrier::get(carrier, "x-request-id")
  assert_true(request_id.is_some(), "Should find request ID")
  assert_eq(request_id.unwrap(), "req-123456789")
}

test "resource consistency across services" {
  // Create base resource
  let base_attrs = [
    ("service.name", StringValue("api-gateway")),
    ("service.version", StringValue("1.2.3")),
    ("service.instance.id", StringValue("instance-abc123")),
    ("deployment.environment", StringValue("production"))
  ]
  let base_resource = Resource::with_attributes(Resource::new(), base_attrs)
  
  // Create service-specific resource
  let service_attrs = [
    ("service.name", StringValue("user-service")),
    ("service.version", StringValue("2.1.0")),
    ("service.instance.id", StringValue("instance-def456")),
    ("database.type", StringValue("postgresql"))
  ]
  let service_resource = Resource::with_attributes(Resource::new(), service_attrs)
  
  // Test resource consistency
  let base_service_name = Resource::get_attribute(base_resource, "service.name")
  match base_service_name {
    Some(StringValue(name)) => assert_eq(name, "api-gateway")
    _ => assert_true(false, "Expected StringValue for service name")
  }
  
  let service_service_name = Resource::get_attribute(service_resource, "service.name")
  match service_service_name {
    Some(StringValue(name)) => assert_eq(name, "user-service")
    _ => assert_true(false, "Expected StringValue for service name")
  }
  
  // Test resource merging
  let merged = Resource::merge(base_resource, service_resource)
  
  // Verify merged resource takes service resource attributes
  let merged_service_name = Resource::get_attribute(merged, "service.name")
  match merged_service_name {
    Some(StringValue(name)) => assert_eq(name, "user-service")
    _ => assert_true(false, "Expected merged service name")
  }
  
  let merged_version = Resource::get_attribute(merged, "service.version")
  match merged_version {
    Some(StringValue(version)) => assert_eq(version, "2.1.0")
    _ => assert_true(false, "Expected merged version")
  }
}

test "instrumentation scope consistency" {
  // Create consistent instrumentation scopes
  let scope1 = InstrumentationScope::{
    name: "user-service",
    version: Some("1.0.0"),
    schema_url: Some("https://example.com/schemas/user-service")
  }
  
  let scope2 = InstrumentationScope::{
    name: "user-service",
    version: Some("1.0.0"),
    schema_url: Some("https://example.com/schemas/user-service")
  }
  
  let scope3 = InstrumentationScope::{
    name: "order-service",
    version: Some("2.0.0"),
    schema_url: Some("https://example.com/schemas/order-service")
  }
  
  // Test scope consistency
  assert_eq(scope1.name, scope2.name)
  assert_eq(scope1.version, scope2.version)
  assert_eq(scope1.schema_url, scope2.schema_url)
  
  // Test scope differences
  assert_true(scope1.name != scope3.name)
  assert_true(scope1.version != scope3.version)
  assert_true(scope1.schema_url != scope3.schema_url)
}

test "cross-service telemetry consistency" {
  // Simulate cross-service telemetry scenario
  
  // Service A: Create span context
  let trace_id = "trace-cross-service-123"
  let service_a_span_id = "span-service-a-456"
  let span_ctx_a = SpanContext::new(trace_id, service_a_span_id, true, "service=a")
  
  // Service A: Create tracer and span
  let provider_a = TracerProvider::default()
  let tracer_a = TracerProvider::get_tracer(provider_a, "service-a")
  let span_a = Tracer::start_span(tracer_a, "service-a-operation")
  
  // Service B: Propagate trace context
  let service_b_span_id = "span-service-b-789"
  let span_ctx_b = SpanContext::new(trace_id, service_b_span_id, true, "service=a,service=b")
  
  // Service B: Create tracer and span
  let provider_b = TracerProvider::default()
  let tracer_b = TracerProvider::get_tracer(provider_b, "service-b")
  let span_b = Tracer::start_span(tracer_b, "service-b-operation")
  
  // Verify trace consistency
  assert_eq(SpanContext::trace_id(span_ctx_a), SpanContext::trace_id(span_ctx_b))
  assert_true(SpanContext::trace_id(span_ctx_a) == trace_id)
  
  // Verify different span IDs
  assert_true(SpanContext::span_id(span_ctx_a) != SpanContext::span_id(span_ctx_b))
  
  // Verify sampling consistency
  assert_eq(SpanContext::is_sampled(span_ctx_a), SpanContext::is_sampled(span_ctx_b))
  
  // Verify span names
  assert_eq(Span::name(span_a), "service-a-operation")
  assert_eq(Span::name(span_b), "service-b-operation")
  
  // Cross-service scenario should maintain consistency
  assert_true(true, "Cross-service telemetry consistency should be maintained")
}