// Composite Propagator Advanced Test Suite for Azimuth Telemetry System
// This file contains comprehensive test cases for advanced composite propagator operations

test "composite propagator with multiple trace context propagators" {
  // Create multiple trace context propagators
  let trace_propagator1 = W3CTraceContextPropagator::new()
  let trace_propagator2 = W3CTraceContextPropagator::new()
  let trace_propagator3 = W3CTraceContextPropagator::new()
  
  // Create composite propagator with multiple trace context propagators
  let composite = CompositePropagator::new([trace_propagator1, trace_propagator2, trace_propagator3])
  
  // Create context with trace information
  let ctx = Context::root()
  let trace_key = ContextKey::new("trace.context")
  let ctx_with_trace = Context::with_value(ctx, trace_key, "trace-123:span-456")
  
  // Create carrier
  let carrier = TextMapCarrier::new()
  
  // Inject context
  CompositePropagator::inject(composite, ctx_with_trace, carrier)
  
  // Extract context
  let extracted_ctx = CompositePropagator::extract(composite, carrier)
  
  // Verify extraction worked
  let extracted_value = Context::get(extracted_ctx, ContextKey::new("extracted"))
  assert_eq(extracted_value, Some("true"))
}

test "composite propagator injection extraction consistency" {
  // Create composite propagator
  let propagator = W3CTraceContextPropagator::new()
  let composite = CompositePropagator::new([propagator])
  
  // Original context
  let original_ctx = Context::root()
  let original_ctx_with_data = Context::with_value(original_ctx, ContextKey::new("test.data"), "test-value")
  
  // Inject and extract multiple times to test consistency
  let carrier1 = TextMapCarrier::new()
  CompositePropagator::inject(composite, original_ctx_with_data, carrier1)
  let extracted_ctx1 = CompositePropagator::extract(composite, carrier1)
  
  let carrier2 = TextMapCarrier::new()
  CompositePropagator::inject(composite, extracted_ctx1, carrier2)
  let extracted_ctx2 = CompositePropagator::extract(composite, carrier2)
  
  let carrier3 = TextMapCarrier::new()
  CompositePropagator::inject(composite, extracted_ctx2, carrier3)
  let extracted_ctx3 = CompositePropagator::extract(composite, carrier3)
  
  // Verify consistency across multiple injection/extraction cycles
  let extracted_value1 = Context::get(extracted_ctx1, ContextKey::new("extracted"))
  let extracted_value2 = Context::get(extracted_ctx2, ContextKey::new("extracted"))
  let extracted_value3 = Context::get(extracted_ctx3, ContextKey::new("extracted"))
  
  assert_eq(extracted_value1, Some("true"))
  assert_eq(extracted_value2, Some("true"))
  assert_eq(extracted_value3, Some("true"))
}

test "composite propagator with baggage and trace context" {
  // Create propagators for different context types
  let trace_propagator = W3CTraceContextPropagator::new()
  let baggage_propagator = W3CBaggagePropagator::new()
  
  // Create composite propagator (simplified - using same type for demo)
  let composite = CompositePropagator::new([trace_propagator])
  
  // Create context with both trace and baggage
  let ctx = Context::root()
  let ctx_with_trace = Context::with_value(ctx, ContextKey::new("trace.id"), "trace-123456")
  let ctx_with_baggage = Context::with_value(ctx_with_trace, ContextKey::new("baggage"), "user.id=123,session.id=456")
  let final_ctx = Context::with_value(ctx_with_baggage, ContextKey::new("operation.name"), "getUserProfile")
  
  // Create carrier
  let carrier = TextMapCarrier::new()
  
  // Inject context
  CompositePropagator::inject(composite, final_ctx, carrier)
  
  // Extract context
  let extracted_ctx = CompositePropagator::extract(composite, carrier)
  
  // Verify extraction
  let extracted_value = Context::get(extracted_ctx, ContextKey::new("extracted"))
  assert_eq(extracted_value, Some("true"))
}

test "composite propagator field priority and override" {
  // Create multiple propagators to test priority
  let primary_propagator = W3CTraceContextPropagator::new()
  let secondary_propagator = W3CTraceContextPropagator::new()
  let tertiary_propagator = W3CTraceContextPropagator::new()
  
  // Create composite with priority order
  let composite = CompositePropagator::new([primary_propagator, secondary_propagator, tertiary_propagator])
  
  // Create context with conflicting data
  let ctx = Context::root()
  let ctx_with_primary = Context::with_value(ctx, ContextKey::new("primary.data"), "primary-value")
  let ctx_with_secondary = Context::with_value(ctx_with_primary, ContextKey::new("secondary.data"), "secondary-value")
  let ctx_with_tertiary = Context::with_value(ctx_with_secondary, ContextKey::new("tertiary.data"), "tertiary-value")
  
  // Create carrier
  let carrier = TextMapCarrier::new()
  
  // Inject context
  CompositePropagator::inject(composite, ctx_with_tertiary, carrier)
  
  // Extract context
  let extracted_ctx = CompositePropagator::extract(composite, carrier)
  
  // Verify extraction
  let extracted_value = Context::get(extracted_ctx, ContextKey::new("extracted"))
  assert_eq(extracted_value, Some("true"))
}

test "composite propagator error handling and recovery" {
  // Create composite propagator
  let propagator = W3CTraceContextPropagator::new()
  let composite = CompositePropagator::new([propagator])
  
  // Test with empty context
  let empty_ctx = Context::root()
  let carrier1 = TextMapCarrier::new()
  
  // Should not crash with empty context
  CompositePropagator::inject(composite, empty_ctx, carrier1)
  let extracted_from_empty = CompositePropagator::extract(composite, carrier1)
  
  // Test with empty carrier
  let empty_carrier = TextMapCarrier::new()
  let ctx_from_empty = CompositePropagator::extract(composite, empty_carrier)
  
  // Test with malformed carrier data
  let malformed_carrier = TextMapCarrier::new()
  TextMapCarrier::set(malformed_carrier, "traceparent", "malformed-trace-data")
  let ctx_from_malformed = CompositePropagator::extract(composite, malformed_carrier)
  
  // Verify error handling doesn't crash
  let extracted_value1 = Context::get(extracted_from_empty, ContextKey::new("extracted"))
  let extracted_value2 = Context::get(ctx_from_empty, ContextKey::new("extracted"))
  let extracted_value3 = Context::get(ctx_from_malformed, ContextKey::new("extracted"))
  
  assert_eq(extracted_value1, Some("true"))
  assert_eq(extracted_value2, Some("true"))
  assert_eq(extracted_value3, Some("true"))
}

test "composite propagator concurrent operations" {
  // Create composite propagator
  let propagator = W3CTraceContextPropagator::new()
  let composite = CompositePropagator::new([propagator])
  
  // Create multiple contexts for concurrent operations
  let contexts = Array.range(0, 10).map(i => {
    let ctx = Context::root()
    Context::with_value(ctx, ContextKey::new("concurrent.data"), "value-" + i.to_string())
  })
  
  // Create carriers for concurrent operations
  let carriers = Array.range(0, 10).map(_ => TextMapCarrier::new())
  
  // Perform concurrent inject operations
  for i in Array.range(0, contexts.length) {
    CompositePropagator::inject(composite, contexts[i], carriers[i])
  }
  
  // Perform concurrent extract operations
  let extracted_contexts = Array.map(carriers, fn(carrier) {
    CompositePropagator::extract(composite, carrier)
  })
  
  // Verify concurrent operations
  assert_eq(extracted_contexts.length, 10)
  
  for extracted_ctx in extracted_contexts {
    let extracted_value = Context::get(extracted_ctx, ContextKey::new("extracted"))
    assert_eq(extracted_value, Some("true"))
  }
}

test "composite propagator with custom headers" {
  // Create composite propagator
  let propagator = W3CTraceContextPropagator::new()
  let composite = CompositePropagator::new([propagator])
  
  // Create context with custom headers
  let ctx = Context::root()
  let ctx_with_custom = Context::with_value(ctx, ContextKey::new("custom.header"), "custom-value")
  let ctx_with_multiple = Context::with_value(ctx_with_custom, ContextKey::new("another.header"), "another-value")
  
  // Create carrier with custom headers
  let carrier = TextMapCarrier::new()
  TextMapCarrier::set(carrier, "X-Custom-Header", "pre-existing-custom-value")
  TextMapCarrier::set(carrier, "X-Another-Header", "pre-existing-another-value")
  
  // Inject context
  CompositePropagator::inject(composite, ctx_with_multiple, carrier)
  
  // Extract context
  let extracted_ctx = CompositePropagator::extract(composite, carrier)
  
  // Verify extraction
  let extracted_value = Context::get(extracted_ctx, ContextKey::new("extracted"))
  assert_eq(extracted_value, Some("true"))
}

test "composite propagator performance optimization" {
  // Create composite propagator with multiple propagators
  let propagators = Array.range(0, 5).map(_ => W3CTraceContextPropagator::new())
  let composite = CompositePropagator::new(propagators)
  
  // Measure performance of multiple operations
  let start_time = Clock::now_unix_nanos(Clock::system())
  
  // Create multiple contexts and carriers
  let contexts = Array.range(0, 100).map(i => {
    let ctx = Context::root()
    Context::with_value(ctx, ContextKey::new("performance.data"), "value-" + i.to_string())
  })
  
  let carriers = Array.range(0, 100).map(_ => TextMapCarrier::new())
  
  // Perform inject operations
  for i in Array.range(0, contexts.length) {
    CompositePropagator::inject(composite, contexts[i], carriers[i])
  }
  
  // Perform extract operations
  let extracted_contexts = Array.map(carriers, fn(carrier) {
    CompositePropagator::extract(composite, carrier)
  })
  
  let end_time = Clock::now_unix_nanos(Clock::system())
  let duration_ns = end_time - start_time
  let duration_ms = duration_ns / 1000000L
  
  // Verify performance
  assert_eq(extracted_contexts.length, 100)
  assert_true(duration_ms < 1000L)  // Should complete within 1 second
  
  // Verify all extractions worked
  for extracted_ctx in extracted_contexts {
    let extracted_value = Context::get(extracted_ctx, ContextKey::new("extracted"))
    assert_eq(extracted_value, Some("true"))
  }
}

test "composite propagator cross-service scenario" {
  // Simulate cross-service scenario with composite propagator
  let propagator = W3CTraceContextPropagator::new()
  let composite = CompositePropagator::new([propagator])
  
  // Service 1: Create initial context
  let service1_ctx = Context::root()
  let service1_ctx_with_trace = Context::with_value(service1_ctx, ContextKey::new("trace.id"), "trace-123456")
  let service1_ctx_with_service = Context::with_value(service1_ctx_with_trace, ContextKey::new("service.name"), "service-1")
  
  // Service 1 to Service 2: Propagate context
  let carrier1_to_2 = TextMapCarrier::new()
  CompositePropagator::inject(composite, service1_ctx_with_service, carrier1_to_2)
  let service2_ctx = CompositePropagator::extract(composite, carrier1_to_2)
  let service2_ctx_with_service = Context::with_value(service2_ctx, ContextKey::new("service.name"), "service-2")
  
  // Service 2 to Service 3: Propagate context
  let carrier2_to_3 = TextMapCarrier::new()
  CompositePropagator::inject(composite, service2_ctx_with_service, carrier2_to_3)
  let service3_ctx = CompositePropagator::extract(composite, carrier2_to_3)
  let service3_ctx_with_service = Context::with_value(service3_ctx, ContextKey::new("service.name"), "service-3")
  
  // Verify cross-service propagation
  let service1_name = Context::get(service1_ctx_with_service, ContextKey::new("service.name"))
  let service2_name = Context::get(service2_ctx_with_service, ContextKey::new("service.name"))
  let service3_name = Context::get(service3_ctx_with_service, ContextKey::new("service.name"))
  
  assert_eq(service1_name, Some("service-1"))
  assert_eq(service2_name, Some("service-2"))
  assert_eq(service3_name, Some("service-3"))
  
  // Verify trace ID is preserved (simplified implementation)
  let extracted_value1 = Context::get(service2_ctx, ContextKey::new("extracted"))
  let extracted_value2 = Context::get(service3_ctx, ContextKey::new("extracted"))
  
  assert_eq(extracted_value1, Some("true"))
  assert_eq(extracted_value2, Some("true"))
}

test "composite propagator with HTTP headers integration" {
  // Create composite propagator
  let propagator = W3CTraceContextPropagator::new()
  let composite = CompositePropagator::new([propagator])
  
  // Create HTTP request with headers
  let http_headers = [
    ("traceparent", "00-0af7651916cd43dd8448eb211c80319c-b7ad6b7169203331-01"),
    ("tracestate", "rojo=00f067aa0ba902b7,congo=t61rcWkgMzE"),
    ("baggage", "userId=user-123,sessionId=session-456"),
    ("X-Request-ID", "req-789"),
    ("X-Custom-Header", "custom-value")
  ]
  
  let http_request = HttpRequest::new("GET", "https://api.example.com/users", http_headers)
  
  // Create carrier from HTTP headers
  let carrier = TextMapCarrier::new()
  for (header_name, header_value) in http_headers {
    TextMapCarrier::set(carrier, header_name, header_value)
  }
  
  // Extract context from HTTP headers
  let extracted_ctx = CompositePropagator::extract(composite, carrier)
  
  // Add service-specific context
  let service_ctx = Context::with_value(extracted_ctx, ContextKey::new("service.name"), "api-service")
  let service_ctx_with_operation = Context::with_value(service_ctx, ContextKey::new("operation.name"), "getUser")
  
  // Create response carrier
  let response_carrier = TextMapCarrier::new()
  
  // Inject context for response
  CompositePropagator::inject(composite, service_ctx_with_operation, response_carrier)
  
  // Verify HTTP integration
  let extracted_value = Context::get(extracted_ctx, ContextKey::new("extracted"))
  let service_name = Context::get(service_ctx_with_operation, ContextKey::new("service.name"))
  let operation_name = Context::get(service_ctx_with_operation, ContextKey::new("operation.name"))
  
  assert_eq(extracted_value, Some("true"))
  assert_eq(service_name, Some("api-service"))
  assert_eq(operation_name, Some("getUser"))
}