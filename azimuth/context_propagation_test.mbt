// Context Propagation Test Suite for Azimuth Telemetry System
// Tests context creation, value management, and complex propagation scenarios

test "context creation and basic operations" {
  let ctx = Context::root()
  
  // Test root context has no data
  let key = ContextKey::new("test.key")
  let value = Context::get(ctx, key)
  assert_eq(value, None)
  
  // Test setting and getting values
  let ctx_with_value = Context::with_value(ctx, key, "test.value")
  let retrieved_value = Context::get(ctx_with_value, key)
  assert_eq(retrieved_value, Some("test.value"))
}

test "context with multiple keys and values" {
  let ctx = Context::root()
  
  // Set multiple key-value pairs
  let user_key = ContextKey::new("user.id")
  let ctx1 = Context::with_value(ctx, user_key, "user-12345")
  
  let session_key = ContextKey::new("session.id")
  let ctx2 = Context::with_value(ctx1, session_key, "session-67890")
  
  let request_key = ContextKey::new("request.id")
  let ctx3 = Context::with_value(ctx2, request_key, "req-abc123")
  
  let trace_key = ContextKey::new("trace.id")
  let final_ctx = Context::with_value(ctx3, trace_key, "trace-def456")
  
  // Verify all values exist
  let user_value = Context::get(final_ctx, user_key)
  assert_eq(user_value, Some("user-12345"))
  
  let session_value = Context::get(final_ctx, session_key)
  assert_eq(session_value, Some("session-67890"))
  
  let request_value = Context::get(final_ctx, request_key)
  assert_eq(request_value, Some("req-abc123"))
  
  let trace_value = Context::get(final_ctx, trace_key)
  assert_eq(trace_value, Some("trace-def456"))
}

test "context key creation and uniqueness" {
  let key1 = ContextKey::new("same.key")
  let key2 = ContextKey::new("same.key")
  let key3 = ContextKey::new("different.key")
  
  let ctx = Context::root()
  
  // Set value with first key
  let ctx1 = Context::with_value(ctx, key1, "value1")
  
  // Get with same key name but different instance
  let value1 = Context::get(ctx1, key2)
  assert_eq(value1, Some("value1"))
  
  // Get with different key
  let value2 = Context::get(ctx1, key3)
  assert_eq(value2, None)
}

test "context with special characters in keys and values" {
  let ctx = Context::root()
  
  // Test keys with special characters
  let key1 = ContextKey::new("user-id")
  let key2 = ContextKey::new("user_id")
  let key3 = ContextKey::new("user.id")
  let key4 = ContextKey::new("user/id")
  
  // Test values with special characters
  let ctx1 = Context::with_value(ctx, key1, "user-123")
  let ctx2 = Context::with_value(ctx1, key2, "user_456")
  let ctx3 = Context::with_value(ctx2, key3, "user.789")
  let final_ctx = Context::with_value(ctx3, key4, "user/000")
  
  // Verify special character keys and values
  let value1 = Context::get(final_ctx, key1)
  assert_eq(value1, Some("user-123"))
  
  let value2 = Context::get(final_ctx, key2)
  assert_eq(value2, Some("user_456"))
  
  let value3 = Context::get(final_ctx, key3)
  assert_eq(value3, Some("user.789"))
  
  let value4 = Context::get(final_ctx, key4)
  assert_eq(value4, Some("user/000"))
}

test "context with empty and edge case values" {
  let ctx = Context::root()
  
  // Test empty string value
  let empty_key = ContextKey::new("empty.value")
  let ctx1 = Context::with_value(ctx, empty_key, "")
  
  // Test zero values
  let zero_key = ContextKey::new("zero.value")
  let ctx2 = Context::with_value(ctx1, zero_key, "0")
  
  // Test boolean-like values
  let true_key = ContextKey::new("true.value")
  let ctx3 = Context::with_value(ctx2, true_key, "true")
  
  let false_key = ContextKey::new("false.value")
  let ctx4 = Context::with_value(ctx3, false_key, "false")
  
  // Test numeric values as strings
  let numeric_key = ContextKey::new("numeric.value")
  let final_ctx = Context::with_value(ctx4, numeric_key, "123.456")
  
  // Verify edge case values
  let empty_value = Context::get(final_ctx, empty_key)
  assert_eq(empty_value, Some(""))
  
  let zero_value = Context::get(final_ctx, zero_key)
  assert_eq(zero_value, Some("0"))
  
  let true_value = Context::get(final_ctx, true_key)
  assert_eq(true_value, Some("true"))
  
  let false_value = Context::get(final_ctx, false_key)
  assert_eq(false_value, Some("false"))
  
  let numeric_value = Context::get(final_ctx, numeric_key)
  assert_eq(numeric_value, Some("123.456"))
}

test "context value overwrite" {
  let ctx = Context::root()
  let key = ContextKey::new("overwrite.key")
  
  // Set initial value
  let ctx1 = Context::with_value(ctx, key, "initial.value")
  let initial_value = Context::get(ctx1, key)
  assert_eq(initial_value, Some("initial.value"))
  
  // Overwrite with new value
  let ctx2 = Context::with_value(ctx1, key, "overwritten.value")
  let overwritten_value = Context::get(ctx2, key)
  assert_eq(overwritten_value, Some("overwritten.value"))
  
  // Overwrite again
  let ctx3 = Context::with_value(ctx2, key, "final.value")
  let final_value = Context::get(ctx3, key)
  assert_eq(final_value, Some("final.value"))
}

test "context with very long keys and values" {
  let ctx = Context::root()
  
  // Test long key
  let long_key = ContextKey::new("this.is.a.very.long.key.name.that.exceeds.normal.lengths.for.testing.purposes")
  let ctx1 = Context::with_value(ctx, long_key, "value1")
  
  // Test long value
  let long_value = "this.is.a.very.long.value.that.exceeds.normal.lengths.for.testing.purposes.and.should.be.preserved.correctly"
  let long_value_key = ContextKey::new("long.value.key")
  let ctx2 = Context::with_value(ctx1, long_value_key, long_value)
  
  // Test both long key and value
  let final_ctx = Context::with_value(ctx2, long_key, long_value)
  
  // Verify long keys and values
  let long_key_value = Context::get(final_ctx, long_key)
  assert_eq(long_key_value, Some(long_value))
  
  let long_value_result = Context::get(final_ctx, long_value_key)
  assert_eq(long_value_result, Some(long_value))
}

test "context propagation through function calls" {
  let ctx = Context::root()
  let correlation_key = ContextKey::new("correlation.id")
  let ctx_with_correlation = Context::with_value(ctx, correlation_key, "corr-12345")
  
  // Simulate function call with context
  fn process_request(ctx : Context, request_id : String) -> String {
    let req_key = ContextKey::new("request.id")
    let ctx_with_request = Context::with_value(ctx, req_key, request_id)
    
    // Get correlation ID from context
    let corr_value = Context::get(ctx_with_request, correlation_key)
    match corr_value {
      Some(corr_id) => "Processed " + request_id + " with correlation " + corr_id
      None => "Processed " + request_id + " without correlation"
    }
  }
  
  let result = process_request(ctx_with_correlation, "req-67890")
  assert_eq(result, "Processed req-67890 with correlation corr-12345")
}

test "context with hierarchical data" {
  let ctx = Context::root()
  
  // Create hierarchical context structure
  let service_key = ContextKey::new("service.name")
  let ctx1 = Context::with_value(ctx, service_key, "authentication-service")
  
  let operation_key = ContextKey::new("operation.name")
  let ctx2 = Context::with_value(ctx1, operation_key, "validate-token")
  
  let user_key = ContextKey::new("user.id")
  let ctx3 = Context::with_value(ctx2, user_key, "user-12345")
  
  let session_key = ContextKey::new("session.id")
  let ctx4 = Context::with_value(ctx3, session_key, "session-67890")
  
  let tenant_key = ContextKey::new("tenant.id")
  let final_ctx = Context::with_value(ctx4, tenant_key, "tenant-abc")
  
  // Verify hierarchical context access
  let service_name = Context::get(final_ctx, service_key)
  assert_eq(service_name, Some("authentication-service"))
  
  let operation_name = Context::get(final_ctx, operation_key)
  assert_eq(operation_name, Some("validate-token"))
  
  let user_id = Context::get(final_ctx, user_key)
  assert_eq(user_id, Some("user-12345"))
  
  let session_id = Context::get(final_ctx, session_key)
  assert_eq(session_id, Some("session-67890"))
  
  let tenant_id = Context::get(final_ctx, tenant_key)
  assert_eq(tenant_id, Some("tenant-abc"))
}

test "context with JSON-like values" {
  let ctx = Context::root()
  
  // Test JSON-like string values
  let user_data_key = ContextKey::new("user.data")
  let user_data = "{\"id\": \"user-123\", \"name\": \"John Doe\", \"email\": \"john@example.com\"}"
  let ctx1 = Context::with_value(ctx, user_data_key, user_data)
  
  let config_key = ContextKey::new("service.config")
  let config = "{\"timeout\": 30, \"retries\": 3, \"endpoints\": [\"api1\", \"api2\"]}"
  let ctx2 = Context::with_value(ctx1, config_key, config)
  
  let metadata_key = ContextKey::new("request.metadata")
  let metadata = "{\"timestamp\": \"2023-01-01T00:00:00Z\", \"version\": \"v1.2.3\", \"environment\": \"production\"}"
  let final_ctx = Context::with_value(ctx2, metadata_key, metadata)
  
  // Verify JSON-like values
  let retrieved_user_data = Context::get(final_ctx, user_data_key)
  assert_eq(retrieved_user_data, Some(user_data))
  
  let retrieved_config = Context::get(final_ctx, config_key)
  assert_eq(retrieved_config, Some(config))
  
  let retrieved_metadata = Context::get(final_ctx, metadata_key)
  assert_eq(retrieved_metadata, Some(metadata))
}

test "context chain and inheritance" {
  let ctx = Context::root()
  
  // Build context chain
  let global_key = ContextKey::new("global.setting")
  let ctx1 = Context::with_value(ctx, global_key, "global-value")
  
  let service_key = ContextKey::new("service.setting")
  let ctx2 = Context::with_value(ctx1, service_key, "service-value")
  
  let request_key = ContextKey::new("request.setting")
  let ctx3 = Context::with_value(ctx2, request_key, "request-value")
  
  let operation_key = ContextKey::new("operation.setting")
  let final_ctx = Context::with_value(ctx3, operation_key, "operation-value")
  
  // Verify context inheritance - all values should be accessible
  let global_value = Context::get(final_ctx, global_key)
  assert_eq(global_value, Some("global-value"))
  
  let service_value = Context::get(final_ctx, service_key)
  assert_eq(service_value, Some("service-value"))
  
  let request_value = Context::get(final_ctx, request_key)
  assert_eq(request_value, Some("request-value"))
  
  let operation_value = Context::get(final_ctx, operation_key)
  assert_eq(operation_value, Some("operation-value"))
  
  // Verify intermediate contexts have their respective values
  let ctx2_global = Context::get(ctx2, global_key)
  assert_eq(ctx2_global, Some("global-value"))
  
  let ctx2_service = Context::get(ctx2, service_key)
  assert_eq(ctx2_service, Some("service-value"))
  
  let ctx2_request = Context::get(ctx2, request_key)
  assert_eq(ctx2_request, None)  // Should not have request value yet
}