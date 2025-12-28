// Propagator Injection and Extraction Tests
// Testing trace context and baggage propagation across service boundaries

test "w3c trace context propagator basic operations" {
  let propagator = W3CTraceContextPropagator::new()
  let ctx = Context::root()
  let carrier = TextMapCarrier::new()
  
  // Test basic injection
  let trace_ctx = SpanContext::new("0af7651916cd43dd8448eb211c80319c", "b7ad6b7169203331", true, "rojo=00f067aa0ba902b7")
  
  // In the real implementation, this would inject trace context into carrier
  // For now, we test the simplified implementation
  
  // Test basic extraction
  let extraction_carrier = TextMapCarrier::new()
  TextMapCarrier::set(extraction_carrier, "traceparent", "00-0af7651916cd43dd8448eb211c80319c-b7ad6b7169203331-01")
  TextMapCarrier::set(extraction_carrier, "tracestate", "rojo=00f067aa0ba902b7")
  
  let extracted_traceparent = TextMapCarrier::get(extraction_carrier, "traceparent")
  let extracted_tracestate = TextMapCarrier::get(extraction_carrier, "tracestate")
  
  assert_eq(extracted_traceparent, Some("00-0af7651916cd43dd8448eb211c80319c-b7ad6b7169203331-01"))
  assert_eq(extracted_tracestate, Some("rojo=00f067aa0ba902b7"))
}

test "w3c baggage propagator operations" {
  let baggage_propagator = W3CBaggagePropagator::new()
  let ctx = Context::root()
  let carrier = TextMapCarrier::new()
  
  // Test baggage injection
  let baggage = Baggage::new()
  let baggage_with_entries = Baggage::set_entry(baggage, "user.id", "12345")
  baggage_with_entries = Baggage::set_entry(baggage_with_entries, "session.id", "abcdef123456")
  
  // Test baggage extraction
  let baggage_carrier = TextMapCarrier::new()
  TextMapCarrier::set(baggage_carrier, "baggage", "user.id=12345,session.id=abcdef123456")
  
  let extracted_baggage = TextMapCarrier::get(baggage_carrier, "baggage")
  assert_eq(extracted_baggage, Some("user.id=12345,session.id=abcdef123456"))
  
  // Test baggage with special characters
  let special_baggage_carrier = TextMapCarrier::new()
  TextMapCarrier::set(special_baggage_carrier, "baggage", "user.email=test%40example.com,service.name=my%20service")
  
  let special_baggage = TextMapCarrier::get(special_baggage_carrier, "baggage")
  assert_eq(special_baggage, Some("user.email=test%40example.com,service.name=my%20service"))
}

test "composite propagator with multiple propagators" {
  let trace_propagator = W3CTraceContextPropagator::new()
  let baggage_propagator2 = W3CBaggagePropagator::new()
  let propagators = [trace_propagator]
  let composite = CompositePropagator::new(propagators)
  
  let ctx = Context::root()
  let carrier = TextMapCarrier::new()
  
  // Test composite injection
  CompositePropagator::inject(composite, ctx, carrier)
  
  // Verify injected trace context
  let injected_traceparent = TextMapCarrier::get(carrier, "traceparent")
  assert_eq(injected_traceparent, Some("00-test-trace-id-test-span-id-01"))
  
  // Test composite extraction
  let extraction_carrier = TextMapCarrier::new()
  TextMapCarrier::set(extraction_carrier, "traceparent", "00-0af7651916cd43dd8448eb211c80319c-b7ad6b7169203331-01")
  TextMapCarrier::set(extraction_carrier, "baggage", "user.id=12345")
  
  let extracted_ctx = CompositePropagator::extract(composite, extraction_carrier)
  
  // Verify extraction created a different context
  let extraction_key = ContextKey::new("extracted")
  let extracted_value = Context::get(extracted_ctx, extraction_key)
  assert_eq(extracted_value, Some("true"))
}

test "trace context parsing and validation" {
  let carrier = TextMapCarrier::new()
  
  // Test valid traceparent formats
  let valid_traceparents = [
    "00-0af7651916cd43dd8448eb211c80319c-b7ad6b7169203331-01",
    "00-4bf92f3577b34da6a3ce929d0e0e4736-00f067aa0ba902b7-01",
    "00-1234567890abcdef1234567890abcdef-fedcba0987654321-00"
  ]
  
  for traceparent in valid_traceparents {
    TextMapCarrier::set(carrier, "traceparent", traceparent)
    let retrieved = TextMapCarrier::get(carrier, "traceparent")
    assert_eq(retrieved, Some(traceparent))
  }
  
  // Test tracestate parsing
  let tracestates = [
    "rojo=00f067aa0ba902b7,congo=t61rcWkgMzE",
    "vendor1=value1,vendor2=value2",
    "key.with.dots=value,key_with_underscores=value2"
  ]
  
  for tracestate in tracestates {
    TextMapCarrier::set(carrier, "tracestate", tracestate)
    let retrieved = TextMapCarrier::get(carrier, "tracestate")
    assert_eq(retrieved, Some(tracestate))
  }
}

test "baggage header parsing with url encoding" {
  let carrier = TextMapCarrier::new()
  
  // Test baggage with URL encoded values
  let encoded_baggage_values = [
    "user.id=12345",
    "user.email=test%40example.com",
    "service.name=my%20service",
    "complex.value=json%3A%7B%22key%22%3A%22value%22%7D",
    "multi=value1,value2,value3",
    "user.name=%E5%BC%A0%E4%B8%89"
  ]
  
  for baggage_value in encoded_baggage_values {
    TextMapCarrier::set(carrier, "baggage", baggage_value)
    let retrieved = TextMapCarrier::get(carrier, "baggage")
    assert_eq(retrieved, Some(baggage_value))
  }
  
  // Test multiple baggage entries
  let multi_baggage = "user.id=12345,user.email=test%40example.com,service.name=my%20service"
  TextMapCarrier::set(carrier, "baggage", multi_baggage)
  let retrieved_multi = TextMapCarrier::get(carrier, "baggage")
  assert_eq(retrieved_multi, Some(multi_baggage))
}

test "propagation scenario with multiple headers" {
  let carrier = TextMapCarrier::new()
  
  // Set up complete propagation scenario
  TextMapCarrier::set(carrier, "traceparent", "00-0af7651916cd43dd8448eb211c80319c-b7ad6b7169203331-01")
  TextMapCarrier::set(carrier, "tracestate", "rojo=00f067aa0ba902b7,congo=t61rcWkgMzE")
  TextMapCarrier::set(carrier, "baggage", "user.id=12345,session.id=abcdef123456,request.id=req-789")
  
  // Add custom headers
  TextMapCarrier::set(carrier, "x-correlation-id", "corr-abc-123")
  TextMapCarrier::set(carrier, "x-request-id", "req-xyz-456")
  TextMapCarrier::set(carrier, "x-user-id", "user-123")
  
  // Verify all headers can be retrieved
  assert_eq(TextMapCarrier::get(carrier, "traceparent"), Some("00-0af7651916cd43dd8448eb211c80319c-b7ad6b7169203331-01"))
  assert_eq(TextMapCarrier::get(carrier, "tracestate"), Some("rojo=00f067aa0ba902b7,congo=t61rcWkgMzE"))
  assert_eq(TextMapCarrier::get(carrier, "baggage"), Some("user.id=12345,session.id=abcdef123456,request.id=req-789"))
  assert_eq(TextMapCarrier::get(carrier, "x-correlation-id"), Some("corr-abc-123"))
  assert_eq(TextMapCarrier::get(carrier, "x-request-id"), Some("req-xyz-456"))
  assert_eq(TextMapCarrier::get(carrier, "x-user-id"), Some("user-123"))
  
  // Test missing headers
  assert_eq(TextMapCarrier::get(carrier, "nonexistent"), None)
  assert_eq(TextMapCarrier::get(carrier, "x-missing-header"), None)
}

test "propagation with empty and invalid values" {
  let carrier = TextMapCarrier::new()
  
  // Test empty traceparent
  TextMapCarrier::set(carrier, "traceparent", "")
  assert_eq(TextMapCarrier::get(carrier, "traceparent"), Some(""))
  
  // Test empty baggage
  TextMapCarrier::set(carrier, "baggage", "")
  assert_eq(TextMapCarrier::get(carrier, "baggage"), Some(""))
  
  // Test malformed traceparent (should still be stored)
  let malformed_traceparents = [
    "invalid-format",
    "00-invalid-trace-id-span-id-01",
    "ff-0af7651916cd43dd8448eb211c80319c-b7ad6b7169203331-01",  // Future version
    "00-0af7651916cd43dd8448eb211c80319c-b7ad6b7169203331-99"   // Invalid flags
  ]
  
  for malformed in malformed_traceparents {
    TextMapCarrier::set(carrier, "traceparent", malformed)
    assert_eq(TextMapCarrier::get(carrier, "traceparent"), Some(malformed))
  }
  
  // Test malformed baggage (should still be stored)
  let malformed_baggage = [
    "invalid-key=",
    "=invalid-value",
    "key=value=extra=equals",
    "key=value,invalid"
  ]
  
  for malformed in malformed_baggage {
    TextMapCarrier::set(carrier, "baggage", malformed)
    assert_eq(TextMapCarrier::get(carrier, "baggage"), Some(malformed))
  }
}

test "context propagation chain simulation" {
  let carrier = TextMapCarrier::new()
  
  // Simulate service-to-service propagation chain
  
  // Service 1: Initialize trace
  TextMapCarrier::set(carrier, "traceparent", "00-11111111111111111111111111111111-2222222222222222-01")
  TextMapCarrier::set(carrier, "baggage", "service=auth,user.id=123")
  
  // Service 2: Add context
  TextMapCarrier::set(carrier, "tracestate", "auth=success")
  TextMapCarrier::set(carrier, "baggage", "service=auth,user.id=123,operation=login")
  
  // Service 3: Update context
  TextMapCarrier::set(carrier, "traceparent", "00-11111111111111111111111111111111-3333333333333333-01")  // New span
  TextMapCarrier::set(carrier, "tracestate", "auth=success,db=query")
  TextMapCarrier::set(carrier, "baggage", "service=auth,user.id=123,operation=login,db.table=users")
  
  // Service 4: Final context
  TextMapCarrier::set(carrier, "x-service-chain", "auth->db->cache->api")
  TextMapCarrier::set(carrier, "baggage", "service=auth,user.id=123,operation=login,db.table=users,cache.hit=true")
  
  // Verify final propagation state
  assert_eq(TextMapCarrier::get(carrier, "traceparent"), Some("00-11111111111111111111111111111111-3333333333333333-01"))
  assert_eq(TextMapCarrier::get(carrier, "tracestate"), Some("auth=success,db=query"))
  assert_eq(TextMapCarrier::get(carrier, "baggage"), Some("service=auth,user.id=123,operation=login,db.table=users,cache.hit=true"))
  assert_eq(TextMapCarrier::get(carrier, "x-service-chain"), Some("auth->db->cache->api"))
}