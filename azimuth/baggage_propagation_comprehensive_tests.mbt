// Baggage Propagation Specialized Test Suite for Azimuth Telemetry System
// This file contains specialized test cases for baggage propagation functionality

test "baggage basic operations" {
  // Test basic baggage operations
  let baggage = Baggage::new()
  
  // Test setting entries
  let baggage_with_key1 = Baggage::set_entry(baggage, "user.id", "12345")
  let baggage_with_key2 = Baggage::set_entry(baggage_with_key1, "request.id", "req-67890")
  let baggage_with_key3 = Baggage::set_entry(baggage_with_key2, "service.version", "1.2.3")
  
  // Test getting entries
  let user_id = Baggage::get_entry(baggage_with_key3, "user.id")
  let request_id = Baggage::get_entry(baggage_with_key3, "request.id")
  let service_version = Baggage::get_entry(baggage_with_key3, "service.version")
  let missing_entry = Baggage::get_entry(baggage_with_key3, "missing.key")
  
  // Verify entries
  assert_eq(user_id, Some("12345"))
  assert_eq(request_id, Some("req-67890"))
  assert_eq(service_version, Some("1.2.3"))
  assert_eq(missing_entry, None)
}

test "baggage entry removal" {
  // Test baggage entry removal functionality
  let baggage = Baggage::new()
  
  // Set multiple entries
  let baggage_with_entries = Baggage::set_entry(
    Baggage::set_entry(
      Baggage::set_entry(baggage, "key1", "value1"),
      "key2", 
      "value2"
    ),
    "key3", 
    "value3"
  )
  
  // Verify all entries exist
  assert_eq(Baggage::get_entry(baggage_with_entries, "key1"), Some("value1"))
  assert_eq(Baggage::get_entry(baggage_with_entries, "key2"), Some("value2"))
  assert_eq(Baggage::get_entry(baggage_with_entries, "key3"), Some("value3"))
  
  // Remove an entry
  let baggage_without_key2 = Baggage::remove_entry(baggage_with_entries, "key2")
  
  // Verify removal
  assert_eq(Baggage::get_entry(baggage_without_key2, "key1"), Some("value1"))
  assert_eq(Baggage::get_entry(baggage_without_key2, "key2"), None)
  assert_eq(Baggage::get_entry(baggage_without_key2, "key3"), Some("value3"))
}

test "baggage propagation with context" {
  // Test baggage propagation through context
  let ctx = Context::root()
  let baggage = Baggage::new()
  
  // Create baggage with entries
  let baggage_with_data = Baggage::set_entry(
    Baggage::set_entry(baggage, "correlation.id", "corr-12345"),
    "trace.flags", "01"
  )
  
  // Store baggage in context
  let baggage_key = ContextKey::new("baggage")
  let ctx_with_baggage = Context::with_value(ctx, baggage_key, "correlation.id=corr-12345,trace.flags=01")
  
  // Retrieve baggage from context
  let retrieved_baggage_str = Context::get(ctx_with_baggage, baggage_key)
  assert_eq(retrieved_baggage_str, Some("correlation.id=corr-12345,trace.flags=01"))
  
  // Test individual baggage entries
  assert_eq(Baggage::get_entry(baggage_with_data, "correlation.id"), Some("corr-12345"))
  assert_eq(Baggage::get_entry(baggage_with_data, "trace.flags"), Some("01"))
}

test "baggage with special characters" {
  // Test baggage operations with special characters and encoding
  let baggage = Baggage::new()
  
  // Test entries with special characters
  let baggage_with_special = Baggage::set_entry(baggage, "user.email", "test@example.com")
  let baggage_with_space = Baggage::set_entry(baggage_with_special, "user.name", "John Doe")
  let baggage_with_unicode = Baggage::set_entry(baggage_with_space, "message", "测试消息")
  let baggage_with_emoji = Baggage::set_entry(baggage_with_unicode, "status", "🚀 active")
  
  // Verify special character handling
  assert_eq(Baggage::get_entry(baggage_with_emoji, "user.email"), Some("test@example.com"))
  assert_eq(Baggage::get_entry(baggage_with_emoji, "user.name"), Some("John Doe"))
  assert_eq(Baggage::get_entry(baggage_with_emoji, "message"), Some("测试消息"))
  assert_eq(Baggage::get_entry(baggage_with_emoji, "status"), Some("🚀 active"))
}

test "baggage cross service propagation" {
  // Test baggage propagation across service boundaries
  let baggage = Baggage::new()
  
  // Service A sets baggage
  let service_a_baggage = Baggage::set_entry(
    Baggage::set_entry(
      Baggage::set_entry(baggage, "service.a.id", "svc-a-001"),
      "user.session", "sess-12345"
    ),
    "request.start", "2025-12-28T10:00:00Z"
  )
  
  // Service B adds to baggage
  let service_b_baggage = Baggage::set_entry(
    service_a_baggage,
    "service.b.id", "svc-b-002"
  )
  
  // Service C adds to baggage
  let service_c_baggage = Baggage::set_entry(
    service_b_baggage,
    "service.c.id", "svc-c-003"
  )
  
  // Verify all service entries are preserved
  assert_eq(Baggage::get_entry(service_c_baggage, "service.a.id"), Some("svc-a-001"))
  assert_eq(Baggage::get_entry(service_c_baggage, "user.session"), Some("sess-12345"))
  assert_eq(Baggage::get_entry(service_c_baggage, "request.start"), Some("2025-12-28T10:00:00Z"))
  assert_eq(Baggage::get_entry(service_c_baggage, "service.b.id"), Some("svc-b-002"))
  assert_eq(Baggage::get_entry(service_c_baggage, "service.c.id"), Some("svc-c-003"))
}

test "baggage size limits and boundary conditions" {
  // Test baggage size limits and boundary conditions
  let baggage = Baggage::new()
  
  // Test empty baggage
  let empty_entry = Baggage::get_entry(baggage, "any.key")
  assert_eq(empty_entry, None)
  
  // Test very long keys and values
  let very_long_key = "this.is.a.very.long.key.name.that.exceeds.normal.limits.and.tests.boundary.conditions"
  let very_long_value = "this.is.a.very.long.value.that.exceeds.normal.limits.and.tests.boundary.conditions.for.baggage.values"
  let baggage_with_long = Baggage::set_entry(baggage, very_long_key, very_long_value)
  
  assert_eq(Baggage::get_entry(baggage_with_long, very_long_key), Some(very_long_value))
  
  // Test empty key and value
  let baggage_with_empty_key = Baggage::set_entry(baggage, "", "empty.key.test")
  let baggage_with_empty_value = Baggage::set_entry(baggage_with_empty_key, "empty.value.test", "")
  
  assert_eq(Baggage::get_entry(baggage_with_empty_value, ""), Some("empty.key.test"))
  assert_eq(Baggage::get_entry(baggage_with_empty_value, "empty.value.test"), Some(""))
}

test "baggage concurrent operations" {
  // Test concurrent baggage operations
  let baggage = Baggage::new()
  
  // Simulate concurrent baggage modifications
  let baggage1 = Baggage::set_entry(baggage, "concurrent.key1", "value1")
  let baggage2 = Baggage::set_entry(baggage, "concurrent.key2", "value2")
  let baggage3 = Baggage::set_entry(baggage, "concurrent.key3", "value3")
  
  // Merge baggage from different operations (simplified simulation)
  let merged_baggage = Baggage::set_entry(
    Baggage::set_entry(
      Baggage::set_entry(baggage1, "concurrent.key2", "value2"),
      "concurrent.key3", 
      "value3"
    ),
    "concurrent.key4", 
    "value4"
  )
  
  // Verify merged baggage
  assert_eq(Baggage::get_entry(merged_baggage, "concurrent.key1"), Some("value1"))
  assert_eq(Baggage::get_entry(merged_baggage, "concurrent.key2"), Some("value2"))
  assert_eq(Baggage::get_entry(merged_baggage, "concurrent.key3"), Some("value3"))
  assert_eq(Baggage::get_entry(merged_baggage, "concurrent.key4"), Some("value4"))
}

test "baggage integration with tracing" {
  // Test baggage integration with tracing system
  let tracer_provider = TracerProvider::default()
  let tracer = TracerProvider::get_tracer(tracer_provider, "baggage.test")
  
  // Create span with baggage
  let span = Tracer::start_span(tracer, "baggage.integration.test")
  let span_context = Span::span_context(span)
  
  // Create baggage with trace context
  let baggage = Baggage::new()
  let baggage_with_trace = Baggage::set_entry(
    Baggage::set_entry(
      baggage, 
      "trace.id", 
      SpanContext::trace_id(span_context)
    ),
    "span.id", 
    SpanContext::span_id(span_context)
  )
  
  // Add business context to baggage
  let baggage_with_business = Baggage::set_entry(
    baggage_with_trace,
    "business.transaction.id",
    "txn-12345"
  )
  
  // Verify baggage contains trace context
  assert_eq(Baggage::get_entry(baggage_with_business, "trace.id"), Some(SpanContext::trace_id(span_context)))
  assert_eq(Baggage::get_entry(baggage_with_business, "span.id"), Some(SpanContext::span_id(span_context)))
  assert_eq(Baggage::get_entry(baggage_with_business, "business.transaction.id"), Some("txn-12345"))
  
  // End span
  Span::end(span)
}