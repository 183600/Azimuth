// Complex Context Propagation Tests
// Testing context propagation through multiple layers and scenarios

test "context propagation through multiple service calls" {
  // Initialize root context
  let root_ctx = Context::root()
  
  // Service 1: Add initial context
  let user_key = ContextKey::new("user.id")
  let ctx1 = Context::with_value(root_ctx, user_key, "user123")
  
  let session_key = ContextKey::new("session.id")
  let ctx2 = Context::with_value(ctx1, session_key, "session456")
  
  // Service 2: Add request context
  let request_key = ContextKey::new("request.id")
  let ctx3 = Context::with_value(ctx2, request_key, "req789")
  
  let operation_key = ContextKey::new("operation.type")
  let ctx4 = Context::with_value(ctx3, operation_key, "user.login")
  
  // Service 3: Add business context
  let tenant_key = ContextKey::new("tenant.id")
  let ctx5 = Context::with_value(ctx4, tenant_key, "tenant001")
  
  let locale_key = ContextKey::new("user.locale")
  let final_ctx = Context::with_value(ctx5, locale_key, "en-US")
  
  // Verify all context values are accessible
  assert_eq(Context::get(final_ctx, user_key), Some("user123"))
  assert_eq(Context::get(final_ctx, session_key), Some("session456"))
  assert_eq(Context::get(final_ctx, request_key), Some("req789"))
  assert_eq(Context::get(final_ctx, operation_key), Some("user.login"))
  assert_eq(Context::get(final_ctx, tenant_key), Some("tenant001"))
  assert_eq(Context::get(final_ctx, locale_key), Some("en-US"))
  
  // Verify missing context values
  let missing_key = ContextKey::new("nonexistent.key")
  assert_eq(Context::get(final_ctx, missing_key), None)
}

test "context value overriding and shadowing" {
  let root_ctx = Context::root()
  
  // Set initial value
  let key = ContextKey::new("shared.key")
  let ctx1 = Context::with_value(root_ctx, key, "initial.value")
  
  // Override the value
  let ctx2 = Context::with_value(ctx1, key, "overridden.value")
  
  // Override again
  let ctx3 = Context::with_value(ctx2, key, "final.value")
  
  // Verify only the final value is accessible
  assert_eq(Context::get(ctx3, key), Some("final.value"))
  
  // Verify that different keys don't interfere
  let different_key = ContextKey::new("different.key")
  let ctx4 = Context::with_value(ctx3, different_key, "different.value")
  
  assert_eq(Context::get(ctx4, key), Some("final.value"))
  assert_eq(Context::get(ctx4, different_key), Some("different.value"))
  
  // Verify that keys with similar names are distinct
  let similar_key1 = ContextKey::new("key.similar")
  let similar_key2 = ContextKey::new("key.similar.")  // Different due to trailing dot
  let similar_key3 = ContextKey::new("key.Similar")  // Different due to case
  
  let ctx5 = Context::with_value(ctx4, similar_key1, "value1")
  let ctx6 = Context::with_value(ctx5, similar_key2, "value2")
  let ctx7 = Context::with_value(ctx6, similar_key3, "value3")
  
  assert_eq(Context::get(ctx7, similar_key1), Some("value1"))
  assert_eq(Context::get(ctx7, similar_key2), Some("value2"))
  assert_eq(Context::get(ctx7, similar_key3), Some("value3"))
}

test "context with complex data structures" {
  let root_ctx = Context::root()
  
  // Test context with JSON-like data
  let json_key = ContextKey::new("user.profile")
  let json_value = "{\"id\":123,\"name\":\"John Doe\",\"email\":\"john@example.com\",\"preferences\":{\"theme\":\"dark\",\"notifications\":true}}"
  let ctx1 = Context::with_value(root_ctx, json_key, json_value)
  
  // Test context with comma-separated values
  let csv_key = ContextKey::new("user.permissions")
  let csv_value = "read,write,delete,admin"
  let ctx2 = Context::with_value(ctx1, csv_key, csv_value)
  
  // Test context with URL-encoded data
  let url_key = ContextKey::new("query.params")
  let url_value = "user=john&action=update&timestamp=1735689600"
  let ctx3 = Context::with_value(ctx2, url_key, url_value)
  
  // Test context with base64 encoded data
  let base64_key = ContextKey::new("binary.data")
  let base64_value = "SGVsbG8gV29ybGQgZnJvbSBiYXNlNjQgZW5jb2Rpbmch"
  let ctx4 = Context::with_value(ctx3, base64_key, base64_value)
  
  // Test context with international characters
  let unicode_key = ContextKey::new("user.name.native")
  let unicode_value = "约翰·史密斯"
  let final_ctx = Context::with_value(ctx4, unicode_key, unicode_value)
  
  // Verify all complex data can be retrieved
  assert_eq(Context::get(final_ctx, json_key), Some(json_value))
  assert_eq(Context::get(final_ctx, csv_key), Some(csv_value))
  assert_eq(Context::get(final_ctx, url_key), Some(url_value))
  assert_eq(Context::get(final_ctx, base64_key), Some(base64_value))
  assert_eq(Context::get(final_ctx, unicode_key), Some(unicode_value))
}

test "context propagation with timing and deadlines" {
  let root_ctx = Context::root()
  
  // Add timing context
  let start_time_key = ContextKey::new("request.start.time")
  let ctx1 = Context::with_value(root_ctx, start_time_key, "1735689600000")
  
  let timeout_key = ContextKey::new("request.timeout.ms")
  let ctx2 = Context::with_value(ctx1, timeout_key, "30000")
  
  // Add deadline context
  let deadline_key = ContextKey::new("request.deadline")
  let ctx3 = Context::with_value(ctx2, deadline_key, "1735718400000")
  
  // Add retry context
  let retry_count_key = ContextKey::new("retry.count")
  let ctx4 = Context::with_value(ctx3, retry_count_key, "0")
  
  let max_retries_key = ContextKey::new("retry.max")
  let ctx5 = Context::with_value(ctx4, max_retries_key, "3")
  
  // Simulate retry scenario
  let retry_ctx1 = Context::with_value(ctx5, retry_count_key, "1")
  let retry_ctx2 = Context::with_value(retry_ctx1, retry_count_key, "2")
  let final_retry_ctx = Context::with_value(retry_ctx2, retry_count_key, "3")
  
  // Verify timing context is preserved through retries
  assert_eq(Context::get(final_retry_ctx, start_time_key), Some("1735689600000"))
  assert_eq(Context::get(final_retry_ctx, timeout_key), Some("30000"))
  assert_eq(Context::get(final_retry_ctx, deadline_key), Some("1735718400000"))
  assert_eq(Context::get(final_retry_ctx, max_retries_key), Some("3"))
  
  // Verify final retry count
  assert_eq(Context::get(final_retry_ctx, retry_count_key), Some("3"))
}

test "context propagation across async boundaries simulation" {
  let root_ctx = Context::root()
  
  // Simulate async operation 1
  let async1_key = ContextKey::new("async.operation.id")
  let ctx1 = Context::with_value(root_ctx, async1_key, "async-001")
  
  let correlation_key = ContextKey::new("correlation.id")
  let ctx2 = Context::with_value(ctx1, correlation_key, "corr-abc-123")
  
  // Simulate async operation 2 (inherits from operation 1)
  let async2_key = ContextKey::new("async.operation.id")
  let ctx3 = Context::with_value(ctx2, async2_key, "async-002")
  
  let parent_key = ContextKey::new("parent.operation.id")
  let ctx4 = Context::with_value(ctx3, parent_key, "async-001")
  
  // Simulate async operation 3 (inherits from operation 2)
  let async3_key = ContextKey::new("async.operation.id")
  let ctx5 = Context::with_value(ctx4, async3_key, "async-003")
  
  let parent_key2 = ContextKey::new("parent.operation.id")
  let final_ctx = Context::with_value(ctx5, parent_key2, "async-002")
  
  // Verify async chain context
  assert_eq(Context::get(final_ctx, correlation_key), Some("corr-abc-123"))
  assert_eq(Context::get(final_ctx, async3_key), Some("async-003"))
  assert_eq(Context::get(final_ctx, parent_key2), Some("async-002"))
  
  // Verify that earlier async context is not directly accessible
  assert_eq(Context::get(final_ctx, async1_key), None)
  assert_eq(Context::get(final_ctx, async2_key), None)
}

test "context with security and authentication data" {
  let root_ctx = Context::root()
  
  // Add authentication context
  let auth_token_key = ContextKey::new("auth.token")
  let ctx1 = Context::with_value(root_ctx, auth_token_key, "Bearer eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9...")
  
  let user_id_key = ContextKey::new("auth.user.id")
  let ctx2 = Context::with_value(ctx1, user_id_key, "user123")
  
  let user_roles_key = ContextKey::new("auth.user.roles")
  let ctx3 = Context::with_value(ctx2, user_roles_key, "admin,user,reader")
  
  // Add authorization context
  let permissions_key = ContextKey::new("auth.permissions")
  let ctx4 = Context::with_value(ctx3, permissions_key, "read:users,write:posts,delete:comments")
  
  let resource_key = ContextKey::new("auth.resource")
  let ctx5 = Context::with_value(ctx4, resource_key, "/api/v1/users")
  
  // Add security context
  let security_level_key = ContextKey::new("security.level")
  let ctx6 = Context::with_value(ctx5, security_level_key, "high")
  
  let ip_address_key = ContextKey::new("security.client.ip")
  let final_ctx = Context::with_value(ctx6, ip_address_key, "192.168.1.100")
  
  // Verify security context
  assert_eq(Context::get(final_ctx, auth_token_key), Some("Bearer eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9..."))
  assert_eq(Context::get(final_ctx, user_id_key), Some("user123"))
  assert_eq(Context::get(final_ctx, user_roles_key), Some("admin,user,reader"))
  assert_eq(Context::get(final_ctx, permissions_key), Some("read:users,write:posts,delete:comments"))
  assert_eq(Context::get(final_ctx, resource_key), Some("/api/v1/users"))
  assert_eq(Context::get(final_ctx, security_level_key), Some("high"))
  assert_eq(Context::get(final_ctx, ip_address_key), Some("192.168.1.100"))
}

test "context propagation with business logic flow" {
  let root_ctx = Context::root()
  
  // Order processing flow
  let order_id_key = ContextKey::new("order.id")
  let ctx1 = Context::with_value(root_ctx, order_id_key, "ORDER-12345")
  
  let customer_id_key = ContextKey::new("customer.id")
  let ctx2 = Context::with_value(ctx1, customer_id_key, "CUST-67890")
  
  // Inventory check
  let product_id_key = ContextKey::new("product.id")
  let ctx3 = Context::with_value(ctx2, product_id_key, "PROD-11111")
  
  let inventory_status_key = ContextKey::new("inventory.status")
  let ctx4 = Context::with_value(ctx3, inventory_status_key, "available")
  
  // Payment processing
  let payment_id_key = ContextKey::new("payment.id")
  let ctx5 = Context::with_value(ctx4, payment_id_key, "PAY-22222")
  
  let payment_status_key = ContextKey::new("payment.status")
  let ctx6 = Context::with_value(ctx5, payment_status_key, "completed")
  
  // Shipping
  let shipping_id_key = ContextKey::new("shipping.id")
  let ctx7 = Context::with_value(ctx6, shipping_id_key, "SHIP-33333")
  
  let shipping_status_key = ContextKey::new("shipping.status")
  let final_ctx = Context::with_value(ctx7, shipping_status_key, "pending")
  
  // Verify complete business flow context
  assert_eq(Context::get(final_ctx, order_id_key), Some("ORDER-12345"))
  assert_eq(Context::get(final_ctx, customer_id_key), Some("CUST-67890"))
  assert_eq(Context::get(final_ctx, product_id_key), Some("PROD-11111"))
  assert_eq(Context::get(final_ctx, inventory_status_key), Some("available"))
  assert_eq(Context::get(final_ctx, payment_id_key), Some("PAY-22222"))
  assert_eq(Context::get(final_ctx, payment_status_key), Some("completed"))
  assert_eq(Context::get(final_ctx, shipping_id_key), Some("SHIP-33333"))
  assert_eq(Context::get(final_ctx, shipping_status_key), Some("pending"))
}

test "context propagation error handling and recovery" {
  let root_ctx = Context::root()
  
  // Set up normal context
  let normal_key = ContextKey::new("normal.data")
  let ctx1 = Context::with_value(root_ctx, normal_key, "normal.value")
  
  // Add error context
  let error_type_key = ContextKey::new("error.type")
  let ctx2 = Context::with_value(ctx1, error_type_key, "ValidationError")
  
  let error_message_key = ContextKey::new("error.message")
  let ctx3 = Context::with_value(ctx2, error_message_key, "Invalid input parameter")
  
  let error_code_key = ContextKey::new("error.code")
  let ctx4 = Context::with_value(ctx3, error_code_key, "ERR-400")
  
  // Add recovery context
  let retry_count_key = ContextKey::new("retry.count")
  let ctx5 = Context::with_value(ctx4, retry_count_key, "1")
  
  let fallback_used_key = ContextKey::new("fallback.used")
  let ctx6 = Context::with_value(ctx5, fallback_used_key, "true")
  
  // Add resolution context
  let resolution_key = ContextKey::new("error.resolution")
  let final_ctx = Context::with_value(ctx6, resolution_key, "parameter.validation.fixed")
  
  // Verify error handling context
  assert_eq(Context::get(final_ctx, normal_key), Some("normal.value"))
  assert_eq(Context::get(final_ctx, error_type_key), Some("ValidationError"))
  assert_eq(Context::get(final_ctx, error_message_key), Some("Invalid input parameter"))
  assert_eq(Context::get(final_ctx, error_code_key), Some("ERR-400"))
  assert_eq(Context::get(final_ctx, retry_count_key), Some("1"))
  assert_eq(Context::get(final_ctx, fallback_used_key), Some("true"))
  assert_eq(Context::get(final_ctx, resolution_key), Some("parameter.validation.fixed"))
}