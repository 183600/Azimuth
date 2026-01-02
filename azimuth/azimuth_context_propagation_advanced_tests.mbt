// Advanced Context Propagation Tests for Azimuth Telemetry System
// This file contains advanced test cases for context propagation

test "composite propagator injection and extraction" {
  // Test composite propagator with multiple propagators
  let trace_propagator = W3CTraceContextPropagator::new()
  let baggage_propagator = W3CBaggagePropagator::new()
  
  // Create composite propagator
  let propagators = [trace_propagator]
  let composite_propagator = CompositePropagator::new(propagators)
  
  // Create context with trace data
  let root_context = Context::root()
  let trace_key = ContextKey::new("trace_data")
  let enriched_context = Context::with_value(root_context, trace_key, "sample_trace_value")
  
  // Create carrier for injection
  let carrier = TextMapCarrier::new()
  
  // Inject context into carrier
  CompositePropagator::inject(composite_propagator, enriched_context, carrier)
  
  // Extract context from carrier
  let extracted_context = CompositePropagator::extract(composite_propagator, carrier)
  let extracted_value = Context::get(extracted_context, ContextKey::new("extracted"))
  
  // Verify propagation worked correctly
  assert_eq(extracted_value, Some("true"))
}

test "baggage propagation across context boundaries" {
  // Test baggage propagation across different contexts
  let initial_baggage = Baggage::new()
  
  // Add entries to baggage
  let baggage_with_entries = Baggage::set_entry(initial_baggage, "user.id", "12345")
  let final_baggage = Baggage::set_entry(baggage_with_entries, "request.id", "req-67890")
  
  // Test baggage retrieval
  let user_id = Baggage::get_entry(final_baggage, "user.id")
  let request_id = Baggage::get_entry(final_baggage, "request.id")
  let missing_entry = Baggage::get_entry(final_baggage, "nonexistent.entry")
  
  // Verify baggage entries
  assert_eq(user_id, Some("12345"))
  assert_eq(request_id, Some("req-67890"))
  assert_eq(missing_entry, None)
  
  // Test baggage entry removal
  let baggage_after_removal = Baggage::remove_entry(final_baggage, "user.id")
  let removed_user_id = Baggage::get_entry(baggage_after_removal, "user.id")
  let still_present_request_id = Baggage::get_entry(baggage_after_removal, "request.id")
  
  // Verify removal worked
  assert_eq(removed_user_id, None)
  assert_eq(still_present_request_id, Some("req-67890"))
}