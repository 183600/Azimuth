// Advanced Baggage Operations Test Suite for Azimuth Telemetry System
// This file contains comprehensive test cases for baggage operations and propagation

test "baggage creation and basic operations" {
  // Test baggage creation and basic entry operations
  let baggage = Baggage::new()
  
  // Test setting entries
  let baggage_with_user = Baggage::set_entry(baggage, "user.id", "user123")
  let baggage_with_session = Baggage::set_entry(baggage_with_user, "session.id", "session456")
  let baggage_with_tenant = Baggage::set_entry(baggage_with_session, "tenant.id", "tenant789")
  
  // Test getting entries
  let user_id = Baggage::get_entry(baggage_with_tenant, "user.id")
  let session_id = Baggage::get_entry(baggage_with_tenant, "session.id")
  let tenant_id = Baggage::get_entry(baggage_with_tenant, "tenant.id")
  let missing_entry = Baggage::get_entry(baggage_with_tenant, "missing.key")
  
  // Verify entries
  assert_eq(user_id, Some("user123"))
  assert_eq(session_id, Some("session456"))
  assert_eq(tenant_id, Some("tenant789"))
  assert_eq(missing_entry, None)
}

test "baggage entry removal and update operations" {
  // Test baggage entry removal and update operations
  let baggage = Baggage::new()
  
  // Set initial entries
  let baggage_with_entries = Baggage::set_entry(
    Baggage::set_entry(
      Baggage::set_entry(baggage, "user.id", "user123"),
      "session.id", "session456"
    ),
    "request.id", "req789"
  )
  
  // Test removing entries
  let baggage_without_session = Baggage::remove_entry(baggage_with_entries, "session.id")
  
  // Verify removal
  let user_id = Baggage::get_entry(baggage_without_session, "user.id")
  let session_id = Baggage::get_entry(baggage_without_session, "session.id")
  let request_id = Baggage::get_entry(baggage_without_session, "request.id")
  
  assert_eq(user_id, Some("user123"))
  assert_eq(session_id, None)  // Should be removed
  assert_eq(request_id, Some("req789"))
  
  // Test updating entries (set with same key)
  let updated_baggage = Baggage::set_entry(baggage_without_session, "user.id", "updated_user456")
  let updated_user_id = Baggage::get_entry(updated_baggage, "user.id")
  
  assert_eq(updated_user_id, Some("updated_user456"))
}

test "baggage with special characters and encoding" {
  // Test baggage operations with special characters and encoding
  let baggage = Baggage::new()
  
  // Test entries with special characters
  let special_entries = [
    ("user.email", "test@example.com"),
    ("user.name", "John Doe"),
    ("request.path", "/api/v1/users?filter=active&sort=name"),
    ("trace.context", "service=api-service;version=1.0.0;env=production"),
    ("unicode.value", "测试用户"),
    ("emoji.value", "🚀📊🔍")
  ]
  
  // Add all special entries
  let mut baggage_with_special = baggage
  for entry in special_entries {
    baggage_with_special = Baggage::set_entry(baggage_with_special, entry.0, entry.1)
  }
  
  // Verify all special entries
  let email = Baggage::get_entry(baggage_with_special, "user.email")
  let name = Baggage::get_entry(baggage_with_special, "user.name")
  let path = Baggage::get_entry(baggage_with_special, "request.path")
  let context = Baggage::get_entry(baggage_with_special, "trace.context")
  let unicode = Baggage::get_entry(baggage_with_special, "unicode.value")
  let emoji = Baggage::get_entry(baggage_with_special, "emoji.value")
  
  assert_eq(email, Some("test@example.com"))
  assert_eq(name, Some("John Doe"))
  assert_eq(path, Some("/api/v1/users?filter=active&sort=name"))
  assert_eq(context, Some("service=api-service;version=1.0.0;env=production"))
  assert_eq(unicode, Some("测试用户"))
  assert_eq(emoji, Some("🚀📊🔍"))
}

test "baggage propagation across contexts" {
  // Test baggage propagation across different contexts
  let baggage = Baggage::new()
  
  // Create baggage with correlation data
  let correlation_baggage = Baggage::set_entry(
    Baggage::set_entry(
      Baggage::set_entry(baggage, "correlation.id", "corr-12345"),
      "request.id", "req-67890"
    ),
    "user.session", "sess-abcdef"
  )
  
  // Create contexts with baggage
  let root_ctx = Context::root()
  let ctx_with_correlation = Context::with_value(
    root_ctx,
    ContextKey::new("baggage"),
    "correlation.id=corr-12345;request.id=req-67890;user.session=sess-abcdef"
  )
  
  // Extract baggage from context
  let baggage_value = Context::get(ctx_with_correlation, ContextKey::new("baggage"))
  assert_eq(baggage_value, Some("correlation.id=corr-12345;request.id=req-67890;user.session=sess-abcdef"))
  
  // Test propagation to child context
  let child_ctx = Context::with_value(
    ctx_with_correlation,
    ContextKey::new("child.operation"),
    "process.data"
  )
  
  // Child context should still have baggage
  let child_baggage = Context::get(child_ctx, ContextKey::new("baggage"))
  assert_eq(child_baggage, Some("correlation.id=corr-12345;request.id=req-67890;user.session=sess-abcdef"))
  
  // Child context should have its own data
  let child_operation = Context::get(child_ctx, ContextKey::new("child.operation"))
  assert_eq(child_operation, Some("process.data"))
}

test "baggage size limits and boundary conditions" {
  // Test baggage operations with size limits and boundary conditions
  let baggage = Baggage::new()
  
  // Test with very long keys and values
  let long_key = "this.is.a.very.long.key.name.that.exceeds.normal.limits.and.tests.boundary.conditions"
  let long_value = "this.is.a.very.long.value.that.contains.much.information.and.tests.the.systems.ability.to.handle.large.data.volumes.in.baggage.entries"
  
  let baggage_with_long = Baggage::set_entry(baggage, long_key, long_value)
  let retrieved_long = Baggage::get_entry(baggage_with_long, long_key)
  
  assert_eq(retrieved_long, Some(long_value))
  
  // Test with empty strings
  let baggage_with_empty_key = Baggage::set_entry(baggage, "", "empty.key.test")
  let baggage_with_empty_value = Baggage::set_entry(baggage, "empty.value.test", "")
  
  let empty_key_result = Baggage::get_entry(baggage_with_empty_key, "")
  let empty_value_result = Baggage::get_entry(baggage_with_empty_value, "empty.value.test")
  
  assert_eq(empty_key_result, Some("empty.key.test"))
  assert_eq(empty_value_result, Some(""))
  
  // Test with whitespace-only strings
  let whitespace_key = "   "
  let whitespace_value = "   "
  
  let baggage_with_whitespace = Baggage::set_entry(baggage, whitespace_key, whitespace_value)
  let whitespace_result = Baggage::get_entry(baggage_with_whitespace, whitespace_key)
  
  assert_eq(whitespace_result, Some("   "))
}

test "baggage multi-level propagation" {
  // Test baggage propagation across multiple levels of service calls
  let initial_baggage = Baggage::new()
  
  // Level 1: API Gateway
  let level1_baggage = Baggage::set_entry(
    Baggage::set_entry(initial_baggage, "gateway.request.id", "gw-123"),
    "gateway.timestamp", "2025-12-28T10:00:00Z"
  )
  
  // Level 2: Auth Service
  let level2_baggage = Baggage::set_entry(
    Baggage::set_entry(level1_baggage, "auth.user.id", "user-456"),
    "auth.token.id", "token-789"
  )
  
  // Level 3: Business Service
  let level3_baggage = Baggage::set_entry(
    Baggage::set_entry(level2_baggage, "business.operation", "process.order"),
    "business.order.id", "order-101112"
  )
  
  // Level 4: Data Service
  let level4_baggage = Baggage::set_entry(
    Baggage::set_entry(level3_baggage, "data.query.type", "select"),
    "data.table", "orders"
  )
  
  // Verify all levels are preserved
  let gw_request_id = Baggage::get_entry(level4_baggage, "gateway.request.id")
  let gw_timestamp = Baggage::get_entry(level4_baggage, "gateway.timestamp")
  let auth_user_id = Baggage::get_entry(level4_baggage, "auth.user.id")
  let auth_token_id = Baggage::get_entry(level4_baggage, "auth.token.id")
  let business_operation = Baggage::get_entry(level4_baggage, "business.operation")
  let business_order_id = Baggage::get_entry(level4_baggage, "business.order.id")
  let data_query_type = Baggage::get_entry(level4_baggage, "data.query.type")
  let data_table = Baggage::get_entry(level4_baggage, "data.table")
  
  assert_eq(gw_request_id, Some("gw-123"))
  assert_eq(gw_timestamp, Some("2025-12-28T10:00:00Z"))
  assert_eq(auth_user_id, Some("user-456"))
  assert_eq(auth_token_id, Some("token-789"))
  assert_eq(business_operation, Some("process.order"))
  assert_eq(business_order_id, Some("order-101112"))
  assert_eq(data_query_type, Some("select"))
  assert_eq(data_table, Some("orders"))
}

test "baggage with metadata and properties" {
  // Test baggage with metadata and properties for advanced scenarios
  let baggage = Baggage::new()
  
  // Add entries with different types of metadata
  let metadata_baggage = Baggage::set_entry(
    Baggage::set_entry(
      Baggage::set_entry(
        Baggage::set_entry(baggage, "service.name", "payment-service"),
        "service.version", "2.1.0"
      ),
      "service.instance.id", "instance-p-12345"
    ),
    "deployment.environment", "production"
  )
  
  // Add business context metadata
  let business_metadata_baggage = Baggage::set_entry(
    Baggage::set_entry(
      Baggage::set_entry(metadata_baggage, "business.domain", "payments"),
      "business.process", "transaction.processing"
    ),
    "business.priority", "high"
  )
  
  // Add technical metadata
  let technical_metadata_baggage = Baggage::set_entry(
    Baggage::set_entry(
      Baggage::set_entry(business_metadata_baggage, "tech.stack", "moonbit"),
      "tech.framework", "azimuth"
    ),
    "tech.region", "us-west-2"
  )
  
  // Verify all metadata entries
  let service_name = Baggage::get_entry(technical_metadata_baggage, "service.name")
  let service_version = Baggage::get_entry(technical_metadata_baggage, "service.version")
  let service_instance_id = Baggage::get_entry(technical_metadata_baggage, "service.instance.id")
  let deployment_environment = Baggage::get_entry(technical_metadata_baggage, "deployment.environment")
  let business_domain = Baggage::get_entry(technical_metadata_baggage, "business.domain")
  let business_process = Baggage::get_entry(technical_metadata_baggage, "business.process")
  let business_priority = Baggage::get_entry(technical_metadata_baggage, "business.priority")
  let tech_stack = Baggage::get_entry(technical_metadata_baggage, "tech.stack")
  let tech_framework = Baggage::get_entry(technical_metadata_baggage, "tech.framework")
  let tech_region = Baggage::get_entry(technical_metadata_baggage, "tech.region")
  
  assert_eq(service_name, Some("payment-service"))
  assert_eq(service_version, Some("2.1.0"))
  assert_eq(service_instance_id, Some("instance-p-12345"))
  assert_eq(deployment_environment, Some("production"))
  assert_eq(business_domain, Some("payments"))
  assert_eq(business_process, Some("transaction.processing"))
  assert_eq(business_priority, Some("high"))
  assert_eq(tech_stack, Some("moonbit"))
  assert_eq(tech_framework, Some("azimuth"))
  assert_eq(tech_region, Some("us-west-2"))
}