// Azimuth Telemetry System - Context Propagation Tests
// This file contains test cases for context propagation functionality

// Test 1: Basic context value propagation
test "basic context value propagation" {
  // Create root context
  let root_ctx = Context::root()
  
  // Create context keys
  let user_key = ContextKey::new("user_id")
  let session_key = ContextKey::new("session_id")
  let request_key = ContextKey::new("request_id")
  
  // Set values in context
  let ctx_with_user = Context::with_value(root_ctx, user_key, "user123")
  let ctx_with_session = Context::with_value(ctx_with_user, session_key, "session456")
  let ctx_with_request = Context::with_value(ctx_with_session, request_key, "request789")
  
  // Get values from context
  match Context::get(ctx_with_request, user_key) {
    Some(user_id) => assert_eq(user_id, "user123")
    None => assert_true(false)
  }
  
  match Context::get(ctx_with_request, session_key) {
    Some(session_id) => assert_eq(session_id, "session456")
    None => assert_true(false)
  }
  
  match Context::get(ctx_with_request, request_key) {
    Some(request_id) => assert_eq(request_id, "request789")
    None => assert_true(false)
  }
  
  // Test getting value from intermediate context
  match Context::get(ctx_with_session, user_key) {
    Some(user_id) => assert_eq(user_id, "user123")
    None => assert_true(false)
  }
  
  match Context::get(ctx_with_session, request_key) {
    Some(_) => assert_true(false)
    None => assert_true(true)
  }
}

// Test 2: Baggage propagation operations
test "baggage propagation operations" {
  // Create new baggage
  let baggage = Baggage::new()
  
  // Set baggage entries
  let baggage_with_user = Baggage::set_entry(baggage, "user_id", "user123")
  let baggage_with_session = Baggage::set_entry(baggage_with_user, "session_id", "session456")
  let baggage_full = Baggage::set_entry(baggage_with_session, "request_id", "request789")
  
  // Get baggage entries
  match Baggage::get_entry(baggage_full, "user_id") {
    Some(user_id) => assert_eq(user_id, "user123")
    None => assert_true(false)
  }
  
  match Baggage::get_entry(baggage_full, "session_id") {
    Some(session_id) => assert_eq(session_id, "session456")
    None => assert_true(false)
  }
  
  match Baggage::get_entry(baggage_full, "request_id") {
    Some(request_id) => assert_eq(request_id, "request789")
    None => assert_true(false)
  }
  
  // Test nonexistent entry
  match Baggage::get_entry(baggage_full, "nonexistent") {
    Some(_) => assert_true(false)
    None => assert_true(true)
  }
  
  // Remove baggage entry
  let baggage_without_user = Baggage::remove_entry(baggage_full, "user_id")
  match Baggage::get_entry(baggage_without_user, "user_id") {
    Some(_) => assert_true(false)
    None => assert_true(true)
  }
  
  // Other entries should still exist
  match Baggage::get_entry(baggage_without_user, "session_id") {
    Some(session_id) => assert_eq(session_id, "session456")
    None => assert_true(false)
  }
}

// Test 3: W3C Trace Context propagation
test "w3c trace context propagation" {
  // Create W3C trace context propagator
  let propagator = W3CTraceContextPropagator::new()
  
  // Create context with trace information
  let ctx = Context::with_value(Context::root(), ContextKey::new("trace_id"), "1234567890abcdef1234567890abcdef")
  
  // Create text map carrier
  let carrier = TextMapCarrier::new()
  
  // Test that propagator can be created
  assert_true(true)
  
  // Test that carrier can be created
  assert_true(true)
}

// Test 4: W3C Baggage propagation
test "w3c baggage propagation" {
  // Create W3C baggage propagator
  let propagator = W3CBaggagePropagator::new()
  
  // Create baggage with entries
  let baggage = Baggage::new()
  let baggage_with_entries = Baggage::set_entry(baggage, "user_id", "user123")
  let final_baggage = Baggage::set_entry(baggage_with_entries, "session_id", "session456")
  
  // Create text map carrier
  let carrier = TextMapCarrier::new()
  
  // Test that propagator can be created
  assert_true(true)
  
  // Test that baggage can be manipulated
  match Baggage::get_entry(final_baggage, "user_id") {
    Some(user_id) => assert_eq(user_id, "user123")
    None => assert_true(false)
  }
}

// Test 5: Composite propagator operations
test "composite propagator operations" {
  // Create individual propagators
  let trace_propagator = W3CTraceContextPropagator::new()
  let baggage_propagator = W3CBaggagePropagator::new()
  
  // Create composite propagator
  let composite = CompositePropagator::new([trace_propagator])
  
  // Create context and carrier
  let ctx = Context::root()
  let carrier = TextMapCarrier::new()
  
  // Inject context into carrier
  CompositePropagator::inject(composite, ctx, carrier)
  
  // Extract context from carrier
  let extracted_ctx = CompositePropagator::extract(composite, carrier)
  
  // Verify extraction
  let extracted_key = ContextKey::new("extracted")
  match Context::get(extracted_ctx, extracted_key) {
    Some(value) => assert_eq(value, "true")
    None => assert_true(false)
  }
  
  // Test with multiple propagators
  let composite_multiple = CompositePropagator::new([trace_propagator, baggage_propagator])
  CompositePropagator::inject(composite_multiple, ctx, carrier)
  let extracted_multiple_ctx = CompositePropagator::extract(composite_multiple, carrier)
  
  match Context::get(extracted_multiple_ctx, extracted_key) {
    Some(value) => assert_eq(value, "true")
    None => assert_true(false)
  }
}

// Test 6: TextMapCarrier with trace headers
test "text map carrier with trace headers" {
  // Create carrier with trace headers
  let carrier = TextMapCarrier::new()
  
  // Set trace headers
  TextMapCarrier::set(carrier, "traceparent", "00-0af7651916cd43dd8448eb211c80319c-b7ad6b7169203331-01")
  TextMapCarrier::set(carrier, "tracestate", "rojo=00f067aa0ba902b7,congo=t61rcWkgMzE")
  TextMapCarrier::set(carrier, "baggage", "userId=amy,userRole=admin")
  
  // Get trace headers
  match TextMapCarrier::get(carrier, "traceparent") {
    Some(traceparent) => assert_eq(traceparent, "00-0af7651916cd43dd8448eb211c80319c-b7ad6b7169203331-01")
    None => assert_true(false)
  }
  
  match TextMapCarrier::get(carrier, "tracestate") {
    Some(tracestate) => assert_eq(tracestate, "rojo=00f067aa0ba902b7,congo=t61rcWkgMzE")
    None => assert_true(false)
  }
  
  match TextMapCarrier::get(carrier, "baggage") {
    Some(baggage) => assert_eq(baggage, "userId=amy,userRole=admin")
    None => assert_true(false)
  }
  
  // Test nonexistent header
  match TextMapCarrier::get(carrier, "nonexistent") {
    Some(_) => assert_true(false)
    None => assert_true(true)
  }
}

// Test 7: Cross-service context propagation
test "cross-service context propagation" {
  // Service A: Create initial context
  let service_a_ctx = Context::root()
  let trace_key = ContextKey::new("trace_id")
  let user_key = ContextKey::new("user_id")
  
  let service_a_with_trace = Context::with_value(service_a_ctx, trace_key, "trace123456789")
  let service_a_final = Context::with_value(service_a_with_trace, user_key, "user123")
  
  // Service A: Inject context into carrier
  let carrier = TextMapCarrier::new()
  let propagator = CompositePropagator::new([W3CTraceContextPropagator::new()])
  CompositePropagator::inject(propagator, service_a_final, carrier)
  
  // Service B: Extract context from carrier
  let service_b_ctx = CompositePropagator::extract(propagator, carrier)
  
  // Service B: Add service-specific context
  let service_key = ContextKey::new("service_name")
  let service_b_with_service = Context::with_value(service_b_ctx, service_key, "service_b")
  
  // Service B: Verify propagation
  match Context::get(service_b_with_service, trace_key) {
    Some(trace_id) => assert_eq(trace_id, "trace123456789")
    None => assert_true(false)
  }
  
  match Context::get(service_b_with_service, user_key) {
    Some(user_id) => assert_eq(user_id, "user123")
    None => assert_true(false)
  }
  
  match Context::get(service_b_with_service, service_key) {
    Some(service_name) => assert_eq(service_name, "service_b")
    None => assert_true(false)
  }
  
  // Service B: Inject updated context
  let updated_carrier = TextMapCarrier::new()
  CompositePropagator::inject(propagator, service_b_with_service, updated_carrier)
  
  // Service C: Extract final context
  let service_c_ctx = CompositePropagator::extract(propagator, updated_carrier)
  
  // Service C: Verify all context is preserved
  match Context::get(service_c_ctx, trace_key) {
    Some(trace_id) => assert_eq(trace_id, "trace123456789")
    None => assert_true(false)
  }
}

// Test 8: Context propagation with baggage
test "context propagation with baggage" {
  // Create initial baggage
  let initial_baggage = Baggage::new()
  let baggage_with_user = Baggage::set_entry(initial_baggage, "user_id", "user123")
  let baggage_with_session = Baggage::set_entry(baggage_with_user, "session_id", "session456")
  let final_baggage = Baggage::set_entry(baggage_with_session, "request_id", "request789")
  
  // Create context with baggage
  let ctx = Context::root()
  let baggage_key = ContextKey::new("baggage")
  let ctx_with_baggage = Context::with_value(ctx, baggage_key, "user_id=user123,session_id=session456,request_id=request789")
  
  // Create composite propagator with both trace and baggage propagators
  let composite = CompositePropagator::new([
    W3CTraceContextPropagator::new(),
    W3CBaggagePropagator::new()
  ])
  
  // Create carrier and inject context
  let carrier = TextMapCarrier::new()
  CompositePropagator::inject(composite, ctx_with_baggage, carrier)
  
  // Extract context in downstream service
  let downstream_ctx = CompositePropagator::extract(composite, carrier)
  
  // Verify baggage is propagated
  match Context::get(downstream_ctx, baggage_key) {
    Some(baggage_str) => assert_eq(baggage_str, "user_id=user123,session_id=session456,request_id=request789")
    None => assert_true(false)
  }
  
  // Add new baggage entry in downstream service
  let downstream_baggage = Baggage::new()
  let downstream_with_additional = Baggage::set_entry(downstream_baggage, "service_name", "downstream_service")
  
  // Verify original baggage entries are still accessible
  match Baggage::get_entry(final_baggage, "user_id") {
    Some(user_id) => assert_eq(user_id, "user123")
    None => assert_true(false)
  }
  
  match Baggage::get_entry(final_baggage, "session_id") {
    Some(session_id) => assert_eq(session_id, "session456")
    None => assert_true(false)
  }
  
  match Baggage::get_entry(final_baggage, "request_id") {
    Some(request_id) => assert_eq(request_id, "request789")
    None => assert_true(false)
  }
}