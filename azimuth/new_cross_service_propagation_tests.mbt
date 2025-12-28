// Cross-Service Propagation Tests for Azimuth
// This file contains tests for tracing context propagation across services

test "basic cross-service trace propagation" {
  // Test basic trace context propagation between services
  let trace_id = "0af7651916cd43dd8448eb211c80319c"
  let span_id = "b7ad6b7169203331"
  let span_ctx = SpanContext::new(trace_id, span_id, true, "key1=value1")
  
  // Create a carrier for propagation
  let carrier = TextMapCarrier::new()
  
  // Inject the context
  TextMapCarrier::set(carrier, "traceparent", "00-0af7651916cd43dd8448eb211c80319c-b7ad6b7169203331-01")
  TextMapCarrier::set(carrier, "tracestate", "key1=value1")
  
  // Extract the context in the target service
  let extracted_traceparent = TextMapCarrier::get(carrier, "traceparent")
  let extracted_tracestate = TextMapCarrier::get(carrier, "tracestate")
  
  assert_eq(extracted_traceparent, Some("00-0af7651916cd43dd8448eb211c80319c-b7ad6b7169203331-01"))
  assert_eq(extracted_tracestate, Some("key1=value1"))
}

test "cross-service baggage propagation" {
  // Test baggage propagation across services
  let baggage = Baggage::new()
  
  // Set baggage entries
  let baggage_with_entries = Baggage::set_entry(baggage, "user.id", "12345")
  let baggage_with_more_entries = Baggage::set_entry(baggage_with_entries, "request.id", "req-67890")
  
  // Create carrier for baggage propagation
  let carrier = TextMapCarrier::new()
  
  // Simulate baggage header format
  TextMapCarrier::set(carrier, "baggage", "user.id=12345,request.id=req-67890")
  
  // Extract baggage in target service
  let baggage_header = TextMapCarrier::get(carrier, "baggage")
  
  assert_eq(baggage_header, Some("user.id=12345,request.id=req-67890"))
}

test "multi-hop service propagation" {
  // Test trace propagation through multiple service hops
  let original_trace_id = "0af7651916cd43dd8448eb211c80319c"
  let original_span_id = "b7ad6b7169203331"
  
  // Service A creates span
  let span_a_ctx = SpanContext::new(original_trace_id, original_span_id, true, "")
  
  // Service A to Service B propagation
  let carrier_ab = TextMapCarrier::new()
  TextMapCarrier::set(carrier_ab, "traceparent", "00-0af7651916cd43dd8448eb211c80319c-b7ad6b7169203331-01")
  
  // Service B creates child span
  let span_b_id = "c9f8e7d6c5b4a321"
  let span_b_ctx = SpanContext::new(original_trace_id, span_b_id, true, "")
  
  // Service B to Service C propagation
  let carrier_bc = TextMapCarrier::new()
  TextMapCarrier::set(carrier_bc, "traceparent", "00-0af7651916cd43dd8448eb211c80319c-c9f8e7d6c5b4a321-01")
  
  // Verify trace ID remains consistent across services
  let trace_ab = TextMapCarrier::get(carrier_ab, "traceparent")
  let trace_bc = TextMapCarrier::get(carrier_bc, "traceparent")
  
  match trace_ab {
    Some(header) => assert_true(header.contains(original_trace_id))
    _ => assert_true(false, "Expected traceparent header")
  }
  
  match trace_bc {
    Some(header) => assert_true(header.contains(original_trace_id))
    _ => assert_true(false, "Expected traceparent header")
  }
}

test "cross-service context with baggage" {
  // Test propagation of both trace context and baggage
  let trace_id = "1234567890abcdef1234567890abcdef"
  let span_id = "fedcba0987654321"
  
  // Create carrier with both trace and baggage
  let carrier = TextMapCarrier::new()
  TextMapCarrier::set(carrier, "traceparent", "00-1234567890abcdef1234567890abcdef-fedcba0987654321-01")
  TextMapCarrier::set(carrier, "baggage", "service=auth,user.id=42,session.id=sess-123")
  TextMapCarrier::set(carrier, "tracestate", "rojo=00f067aa0ba902b7,congo=t61rcWkgMzE")
  
  // Extract all headers in target service
  let traceparent = TextMapCarrier::get(carrier, "traceparent")
  let baggage = TextMapCarrier::get(carrier, "baggage")
  let tracestate = TextMapCarrier::get(carrier, "tracestate")
  
  assert_eq(traceparent, Some("00-1234567890abcdef1234567890abcdef-fedcba0987654321-01"))
  assert_eq(baggage, Some("service=auth,user.id=42,session.id=sess-123"))
  assert_eq(tracestate, Some("rojo=00f067aa0ba902b7,congo=t61rcWkgMzE"))
}

test "cross-service sampling decisions" {
  // Test sampling decision propagation across services
  let trace_id = "11111111111111111111111111111111"
  
  // Test sampled span
  let sampled_span_id = "2222222222222222"
  let sampled_carrier = TextMapCarrier::new()
  TextMapCarrier::set(sampled_carrier, "traceparent", "00-11111111111111111111111111111111-2222222222222222-01")
  
  // Test not sampled span
  let not_sampled_span_id = "3333333333333333"
  let not_sampled_carrier = TextMapCarrier::new()
  TextMapCarrier::set(not_sampled_carrier, "traceparent", "00-11111111111111111111111111111111-3333333333333333-00")
  
  // Extract and verify sampling decisions
  let sampled_header = TextMapCarrier::get(sampled_carrier, "traceparent")
  let not_sampled_header = TextMapCarrier::get(not_sampled_carrier, "traceparent")
  
  match sampled_header {
    Some(header) => assert_true(header.contains("-01"))
    _ => assert_true(false, "Expected sampled traceparent")
  }
  
  match not_sampled_header {
    Some(header) => assert_true(header.contains("-00"))
    _ => assert_true(false, "Expected not sampled traceparent")
  }
}

test "cross-service error propagation" {
  // Test error context propagation across services
  let trace_id = "error-trace-id-1234567890abcdef"
  let span_id = "error-span-id-fedcba0987654321"
  
  // Service A encounters error and propagates context
  let error_carrier = TextMapCarrier::new()
  TextMapCarrier::set(error_carrier, "traceparent", "00-error-trace-id-1234567890abcdef-error-span-id-fedcba0987654321-01")
  TextMapCarrier::set(error_carrier, "baggage", "error.code=500,error.type=InternalError,service=service-a")
  TextMapCarrier::set(error_carrier, "x-error-context", "timeout,database-connection-failed")
  
  // Service B receives error context
  let received_traceparent = TextMapCarrier::get(error_carrier, "traceparent")
  let received_baggage = TextMapCarrier::get(error_carrier, "baggage")
  let received_error_context = TextMapCarrier::get(error_carrier, "x-error-context")
  
  assert_eq(received_traceparent, Some("00-error-trace-id-1234567890abcdef-error-span-id-fedcba0987654321-01"))
  assert_eq(received_baggage, Some("error.code=500,error.type=InternalError,service=service-a"))
  assert_eq(received_error_context, Some("timeout,database-connection-failed"))
}

test "cross-service async operation propagation" {
  // Test context propagation in async operations across services
  let async_trace_id = "async-trace-1234567890abcdef"
  let async_span_id = "async-span-fedcba0987654321"
  
  // Main service initiates async operation
  let async_carrier = TextMapCarrier::new()
  TextMapCarrier::set(async_carrier, "traceparent", "00-async-trace-1234567890abcdef-async-span-fedcba0987654321-01")
  TextMapCarrier::set(async_carrier, "baggage", "operation.type=async,correlation.id=corr-12345")
  TextMapCarrier::set(async_carrier, "x-async-context", "queue=task-queue,priority=high")
  
  // Worker service processes async operation
  let worker_traceparent = TextMapCarrier::get(async_carrier, "traceparent")
  let worker_baggage = TextMapCarrier::get(async_carrier, "baggage")
  let worker_async_context = TextMapCarrier::get(async_carrier, "x-async-context")
  
  assert_eq(worker_traceparent, Some("00-async-trace-1234567890abcdef-async-span-fedcba0987654321-01"))
  assert_eq(worker_baggage, Some("operation.type=async,correlation.id=corr-12345"))
  assert_eq(worker_async_context, Some("queue=task-queue,priority=high"))
}

test "cross-service microservices chain propagation" {
  // Test propagation through a microservices chain
  let chain_trace_id = "chain-trace-1111111111111111"
  
  // API Gateway -> Auth Service -> User Service -> Order Service
  let api_gateway_carrier = TextMapCarrier::new()
  TextMapCarrier::set(api_gateway_carrier, "traceparent", "00-chain-trace-1111111111111111-api-gateway-span-01")
  TextMapCarrier::set(api_gateway_carrier, "baggage", "gateway.version=v2,client.type=web")
  
  // Auth Service adds context
  TextMapCarrier::set(api_gateway_carrier, "baggage", "auth.token.valid=true,auth.user.id=42")
  TextMapCarrier::set(api_gateway_carrier, "traceparent", "00-chain-trace-1111111111111111-auth-service-span-01")
  
  // User Service adds context
  TextMapCarrier::set(api_gateway_carrier, "baggage", "user.tier=premium,user.region=us-west")
  TextMapCarrier::set(api_gateway_carrier, "traceparent", "00-chain-trace-1111111111111111-user-service-span-01")
  
  // Order Service receives full context
  let final_traceparent = TextMapCarrier::get(api_gateway_carrier, "traceparent")
  let final_baggage = TextMapCarrier::get(api_gateway_carrier, "baggage")
  
  match final_traceparent {
    Some(header) => assert_true(header.contains(chain_trace_id))
    _ => assert_true(false, "Expected traceparent header")
  }
  
  match final_baggage {
    Some(baggage_str) => {
      assert_true(baggage_str.contains("gateway.version=v2"))
      assert_true(baggage_str.contains("auth.token.valid=true"))
      assert_true(baggage_str.contains("user.tier=premium"))
    }
    _ => assert_true(false, "Expected baggage header")
  }
}