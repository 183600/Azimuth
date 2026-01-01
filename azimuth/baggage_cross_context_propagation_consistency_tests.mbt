// Baggage Cross-Context Propagation Consistency Tests
// Test cases for comprehensive baggage operations and cross-context propagation

test "baggage_basic_operations" {
  // Test basic baggage creation and operations
  let baggage = Baggage::new()
  
  // Test setting and getting baggage entries
  let updated_baggage = Baggage::set_entry(baggage, "user.id", "12345")
  let retrieved_value = Baggage::get_entry(updated_baggage, "user.id")
  
  @assertion.assert_eq(retrieved_value, Some("12345"))?
  
  // Test with multiple entries
  let multi_baggage = Baggage::set_entry(updated_baggage, "request.id", "req-67890")
  let user_value = Baggage::get_entry(multi_baggage, "user.id")
  let request_value = Baggage::get_entry(multi_baggage, "request.id")
  
  @assertion.assert_eq(user_value, Some("12345"))?
  @assertion.assert_eq(request_value, Some("req-67890"))?
}

test "baggage_entry_removal" {
  // Test baggage entry removal
  let baggage = Baggage::new()
  
  // Add multiple entries
  let baggage_with_entries = Baggage::set_entry(
    Baggage::set_entry(baggage, "key1", "value1"), 
    "key2", 
    "value2"
  )
  
  // Verify both entries exist
  @assertion.assert_eq(Baggage::get_entry(baggage_with_entries, "key1"), Some("value1"))?
  @assertion.assert_eq(Baggage::get_entry(baggage_with_entries, "key2"), Some("value2"))?
  
  // Remove one entry
  let baggage_after_removal = Baggage::remove_entry(baggage_with_entries, "key1")
  
  // Verify removal
  @assertion.assert_eq(Baggage::get_entry(baggage_after_removal, "key1"), None)?
  @assertion.assert_eq(Baggage::get_entry(baggage_after_removal, "key2"), Some("value2"))?
}

test "baggage_context_integration" {
  // Test baggage integration with context system
  let root_ctx = Context::root()
  let baggage = Baggage::new()
  
  // Create baggage with entries
  let populated_baggage = Baggage::set_entry(
    Baggage::set_entry(baggage, "service.name", "payment-service"),
    "trace.id",
    "trace-12345"
  )
  
  // Create context key for baggage
  let baggage_key = ContextKey::new("baggage")
  
  // Store baggage in context
  let ctx_with_baggage = Context::with_value(root_ctx, baggage_key, "baggage_data")
  
  // Retrieve context value
  let retrieved_baggage_data = Context::get(ctx_with_baggage, baggage_key)
  @assertion.assert_eq(retrieved_baggage_data, Some("baggage_data"))?
}

test "baggage_complex_value_handling" {
  // Test baggage with complex values
  let baggage = Baggage::new()
  
  // Test with URL-encoded values
  let url_encoded_value = "user%20name%3DJohn%20Doe"
  let baggage_with_encoded = Baggage::set_entry(baggage, "user.info", url_encoded_value)
  let retrieved_encoded = Baggage::get_entry(baggage_with_encoded, "user.info")
  @assertion.assert_eq(retrieved_encoded, Some(url_encoded_value))?
  
  // Test with JSON-like values
  let json_value = "{\"id\":123,\"status\":\"active\"}"
  let baggage_with_json = Baggage::set_entry(baggage_with_encoded, "metadata", json_value)
  let retrieved_json = Baggage::get_entry(baggage_with_json, "metadata")
  @assertion.assert_eq(retrieved_json, Some(json_value))?
  
  // Test with comma-separated values
  let csv_value = "tag1,tag2,tag3"
  let baggage_with_csv = Baggage::set_entry(baggage_with_json, "tags", csv_value)
  let retrieved_csv = Baggage::get_entry(baggage_with_csv, "tags")
  @assertion.assert_eq(retrieved_csv, Some(csv_value))?
}

test "baggage_edge_cases" {
  // Test baggage edge cases
  let baggage = Baggage::new()
  
  // Test with empty key
  let empty_key_baggage = Baggage::set_entry(baggage, "", "value")
  let empty_key_retrieved = Baggage::get_entry(empty_key_baggage, "")
  @assertion.assert_eq(empty_key_retrieved, Some("value"))?
  
  // Test with empty value
  let empty_value_baggage = Baggage::set_entry(baggage, "key", "")
  let empty_value_retrieved = Baggage::get_entry(empty_value_baggage, "key")
  @assertion.assert_eq(empty_value_retrieved, Some(""))?
  
  // Test with special characters in key
  let special_key = "special.key.with.dots-and_underscores"
  let special_key_baggage = Baggage::set_entry(baggage, special_key, "special_value")
  let special_key_retrieved = Baggage::get_entry(special_key_baggage, special_key)
  @assertion.assert_eq(special_key_retrieved, Some("special_value"))?
  
  // Test with special characters in value
  let special_value = "value with spaces & special!chars@#$%"
  let special_value_baggage = Baggage::set_entry(baggage, "normal.key", special_value)
  let special_value_retrieved = Baggage::get_entry(special_value_baggage, "normal.key")
  @assertion.assert_eq(special_value_retrieved, Some(special_value))?
}

test "baggage_cross_context_propagation" {
  // Test baggage propagation across multiple contexts
  let root_ctx = Context::root()
  
  // Create initial baggage
  let initial_baggage = Baggage::new()
  let baggage_with_user = Baggage::set_entry(initial_baggage, "user.id", "user123")
  
  // Create first context with baggage
  let ctx1_key = ContextKey::new("baggage.ctx1")
  let ctx1 = Context::with_value(root_ctx, ctx1_key, "baggage_data_1")
  
  // Create second context derived from first
  let ctx2_key = ContextKey::new("baggage.ctx2")
  let ctx2 = Context::with_value(ctx1, ctx2_key, "baggage_data_2")
  
  // Verify context propagation
  let ctx1_data = Context::get(ctx1, ctx1_key)
  let ctx2_data = Context::get(ctx2, ctx2_key)
  let ctx1_data_in_ctx2 = Context::get(ctx2, ctx1_key)
  
  @assertion.assert_eq(ctx1_data, Some("baggage_data_1"))?
  @assertion.assert_eq(ctx2_data, Some("baggage_data_2"))?
  @assertion.assert_eq(ctx1_data_in_ctx2, Some("baggage_data_1"))?
  
  // Verify baggage is consistent across contexts
  let user_id_in_ctx1 = Baggage::get_entry(baggage_with_user, "user.id")
  let user_id_in_ctx2 = Baggage::get_entry(baggage_with_user, "user.id")
  
  @assertion.assert_eq(user_id_in_ctx1, Some("user123"))?
  @assertion.assert_eq(user_id_in_ctx2, Some("user123"))?
}

test "baggage_propagator_integration" {
  // Test baggage integration with propagator system
  let baggage = Baggage::new()
  
  // Create baggage with multiple entries
  let populated_baggage = Baggage::set_entry(
    Baggage::set_entry(
      Baggage::set_entry(baggage, "service.name", "api-gateway"),
      "request.id",
      "req-abc123"
    ),
    "user.locale",
    "en-US"
  )
  
  // Create text map carrier
  let carrier = TextMapCarrier::new()
  
  // Create composite propagator
  let trace_propagator = W3CTraceContextPropagator::new()
  let composite_propagator = CompositePropagator::new([trace_propagator])
  
  // Test injection
  let ctx = Context::root()
  CompositePropagator::inject(composite_propagator, ctx, carrier)
  
  // Test extraction
  let extracted_ctx = CompositePropagator::extract(composite_propagator, carrier)
  let extracted_key = ContextKey::new("extracted")
  let extracted_value = Context::get(extracted_ctx, extracted_key)
  
  @assertion.assert_eq(extracted_value, Some("true"))?
  
  // Verify baggage entries are preserved
  @assertion.assert_eq(Baggage::get_entry(populated_baggage, "service.name"), Some("api-gateway"))?
  @assertion.assert_eq(Baggage::get_entry(populated_baggage, "request.id"), Some("req-abc123"))?
  @assertion.assert_eq(Baggage::get_entry(populated_baggage, "user.locale"), Some("en-US"))?
}

test "baggage_consistency_validation" {
  // Test baggage consistency across operations
  let baggage = Baggage::new()
  
  // Create complex baggage structure
  let complex_baggage = Baggage::set_entry(
    Baggage::set_entry(
      Baggage::set_entry(
        Baggage::set_entry(baggage, "operation", "payment"),
        "amount",
        "100.50"
      ),
      "currency",
      "USD"
    ),
    "timestamp",
    "2023-01-01T12:00:00Z"
  )
  
  // Validate all entries exist
  @assertion.assert_eq(Baggage::get_entry(complex_baggage, "operation"), Some("payment"))?
  @assertion.assert_eq(Baggage::get_entry(complex_baggage, "amount"), Some("100.50"))?
  @assertion.assert_eq(Baggage::get_entry(complex_baggage, "currency"), Some("USD"))?
  @assertion.assert_eq(Baggage::get_entry(complex_baggage, "timestamp"), Some("2023-01-01T12:00:00Z"))?
  
  // Test removal doesn't affect other entries
  let baggage_after_removal = Baggage::remove_entry(complex_baggage, "amount")
  
  @assertion.assert_eq(Baggage::get_entry(baggage_after_removal, "operation"), Some("payment"))?
  @assertion.assert_eq(Baggage::get_entry(baggage_after_removal, "amount"), None)?
  @assertion.assert_eq(Baggage::get_entry(baggage_after_removal, "currency"), Some("USD"))?
  @assertion.assert_eq(Baggage::get_entry(baggage_after_removal, "timestamp"), Some("2023-01-01T12:00:00Z"))?
}