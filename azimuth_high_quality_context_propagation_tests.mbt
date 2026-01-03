// High-Quality Test Suite for Context Propagation and Baggage Operations
// This file contains comprehensive test cases for context propagation and baggage operations

test "context creation and root context" {
  let root_ctx = Context::root()
  
  // Root context should have no data
  assert_eq(root_ctx.data, None)
}

test "context with single value" {
  let root_ctx = Context::root()
  let key = ContextKey::new("user.id")
  let ctx_with_value = Context::with_value(root_ctx, key, "user123")
  
  let retrieved_value = Context::get(ctx_with_value, key)
  assert_eq(retrieved_value, Some("user123"))
}

test "context with multiple values" {
  let root_ctx = Context::root()
  
  // Add first value
  let user_key = ContextKey::new("user.id")
  let ctx_with_user = Context::with_value(root_ctx, user_key, "user123")
  
  // Add second value (note: this would normally create a new context chain)
  let session_key = ContextKey::new("session.id")
  let ctx_with_session = Context::with_value(ctx_with_user, session_key, "session456")
  
  // Verify values
  assert_eq(Context::get(ctx_with_session, user_key), Some("user123"))
  assert_eq(Context::get(ctx_with_session, session_key), Some("session456"))
}

test "context key uniqueness" {
  let root_ctx = Context::root()
  
  // Create two different keys with same string
  let key1 = ContextKey::new("test.key")
  let key2 = ContextKey::new("test.key")
  
  let ctx_with_key1 = Context::with_value(root_ctx, key1, "value1")
  let ctx_with_key2 = Context::with_value(ctx_with_key1, key2, "value2")
  
  // Both keys should retrieve the same value since they have the same string
  assert_eq(Context::get(ctx_with_key2, key1), Some("value2"))
  assert_eq(Context::get(ctx_with_key2, key2), Some("value2"))
}

test "context with missing key" {
  let root_ctx = Context::root()
  let key = ContextKey::new("nonexistent.key")
  
  let retrieved_value = Context::get(root_ctx, key)
  assert_eq(retrieved_value, None)
}

test "baggage creation and empty state" {
  let baggage = Baggage::new()
  
  // Empty baggage should have no entries
  assert_eq(baggage.entries.length, 0)
}

test "baggage set and get operations" {
  let baggage = Baggage::new()
  
  // Set a single entry
  let updated_baggage = Baggage::set_entry(baggage, "user.id", "user123")
  
  // Get the entry
  let retrieved_value = Baggage::get_entry(updated_baggage, "user.id")
  assert_eq(retrieved_value, Some("user123"))
}

test "baggage with multiple entries" {
  let baggage = Baggage::new()
  
  // Set multiple entries
  let baggage1 = Baggage::set_entry(baggage, "user.id", "user123")
  let baggage2 = Baggage::set_entry(baggage1, "session.id", "session456")
  let baggage3 = Baggage::set_entry(baggage2, "request.id", "request789")
  
  // Verify all entries exist
  assert_eq(Baggage::get_entry(baggage3, "user.id"), Some("user123"))
  assert_eq(Baggage::get_entry(baggage3, "session.id"), Some("session456"))
  assert_eq(Baggage::get_entry(baggage3, "request.id"), Some("request789"))
}

test "baggage with missing entry" {
  let baggage = Baggage::new()
  
  // Try to get nonexistent entry
  let retrieved_value = Baggage::get_entry(baggage, "nonexistent.key")
  assert_eq(retrieved_value, None)
}

test "baggage entry removal" {
  let baggage = Baggage::new()
  
  // Set entries
  let baggage1 = Baggage::set_entry(baggage, "user.id", "user123")
  let baggage2 = Baggage::set_entry(baggage1, "session.id", "session456")
  
  // Remove one entry
  let baggage3 = Baggage::remove_entry(baggage2, "user.id")
  
  // Verify removal
  assert_eq(Baggage::get_entry(baggage3, "user.id"), None)
  assert_eq(Baggage::get_entry(baggage3, "session.id"), Some("session456"))
}

test "baggage entry update" {
  let baggage = Baggage::new()
  
  // Set initial value
  let baggage1 = Baggage::set_entry(baggage, "user.id", "user123")
  
  // Update the value
  let baggage2 = Baggage::set_entry(baggage1, "user.id", "user456")
  
  // Verify update
  assert_eq(Baggage::get_entry(baggage2, "user.id"), Some("user456"))
}

test "context and baggage integration" {
  let root_ctx = Context::root()
  let baggage = Baggage::new()
  
  // Create context with baggage information
  let ctx_key = ContextKey::new("baggage.user.id")
  let ctx_with_baggage = Context::with_value(root_ctx, ctx_key, "user123")
  
  // Create corresponding baggage entry
  let updated_baggage = Baggage::set_entry(baggage, "user.id", "user123")
  
  // Verify both context and baggage have the same information
  assert_eq(Context::get(ctx_with_baggage, ctx_key), Some("user123"))
  assert_eq(Baggage::get_entry(updated_baggage, "user.id"), Some("user123"))
}

test "context propagation simulation" {
  let root_ctx = Context::root()
  
  // Simulate a service call chain
  let service1_key = ContextKey::new("service1.request.id")
  let ctx_after_service1 = Context::with_value(root_ctx, service1_key, "req123")
  
  let service2_key = ContextKey::new("service2.user.id")
  let ctx_after_service2 = Context::with_value(ctx_after_service1, service2_key, "user456")
  
  let service3_key = ContextKey::new("service3.session.id")
  let ctx_after_service3 = Context::with_value(ctx_after_service2, service3_key, "session789")
  
  // All context values should be preserved through the chain
  assert_eq(Context::get(ctx_after_service3, service1_key), Some("req123"))
  assert_eq(Context::get(ctx_after_service3, service2_key), Some("user456"))
  assert_eq(Context::get(ctx_after_service3, service3_key), Some("session789"))
}

test "baggage across service boundaries" {
  let baggage = Baggage::new()
  
  // Service 1 adds baggage
  let baggage1 = Baggage::set_entry(baggage, "service1.timestamp", "2025-01-01T00:00:00Z")
  let baggage2 = Baggage::set_entry(baggage1, "service1.request.id", "req123")
  
  // Service 2 adds more baggage
  let baggage3 = Baggage::set_entry(baggage2, "service2.user.id", "user456")
  let baggage4 = Baggage::set_entry(baggage3, "service2.operation", "query")
  
  // Service 3 adds final baggage
  let baggage5 = Baggage::set_entry(baggage4, "service3.result", "success")
  let baggage6 = Baggage::set_entry(baggage5, "service3.duration", "150ms")
  
  // Verify all baggage entries are preserved
  assert_eq(Baggage::get_entry(baggage6, "service1.timestamp"), Some("2025-01-01T00:00:00Z"))
  assert_eq(Baggage::get_entry(baggage6, "service1.request.id"), Some("req123"))
  assert_eq(Baggage::get_entry(baggage6, "service2.user.id"), Some("user456"))
  assert_eq(Baggage::get_entry(baggage6, "service2.operation"), Some("query"))
  assert_eq(Baggage::get_entry(baggage6, "service3.result"), Some("success"))
  assert_eq(Baggage::get_entry(baggage6, "service3.duration"), Some("150ms"))
}

test "complex context and baggage scenario" {
  let root_ctx = Context::root()
  let baggage = Baggage::new()
  
  // Initial request context
  let request_key = ContextKey::new("request.id")
  let ctx_with_request = Context::with_value(root_ctx, request_key, "req123")
  
  // Add authentication context
  let auth_key = ContextKey::new("auth.user.id")
  let ctx_with_auth = Context::with_value(ctx_with_request, auth_key, "user456")
  
  // Add baggage for cross-service communication
  let baggage1 = Baggage::set_entry(baggage, "request.id", "req123")
  let baggage2 = Baggage::set_entry(baggage1, "auth.user.id", "user456")
  let baggage3 = Baggage::set_entry(baggage2, "trace.id", "trace789")
  
  // Simulate service processing
  let processing_key = ContextKey::new("processing.start.time")
  let ctx_with_processing = Context::with_value(ctx_with_auth, processing_key, "2025-01-01T12:00:00Z")
  
  let baggage4 = Baggage::set_entry(baggage3, "processing.service", "data-processor")
  
  // Verify all context and baggage values
  assert_eq(Context::get(ctx_with_processing, request_key), Some("req123"))
  assert_eq(Context::get(ctx_with_processing, auth_key), Some("user456"))
  assert_eq(Context::get(ctx_with_processing, processing_key), Some("2025-01-01T12:00:00Z"))
  
  assert_eq(Baggage::get_entry(baggage4, "request.id"), Some("req123"))
  assert_eq(Baggage::get_entry(baggage4, "auth.user.id"), Some("user456"))
  assert_eq(Baggage::get_entry(baggage4, "trace.id"), Some("trace789"))
  assert_eq(Baggage::get_entry(baggage4, "processing.service"), Some("data-processor"))
}