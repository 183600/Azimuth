// Comprehensive Baggage Propagation Test Suite
// Tests for baggage creation, entry management, and propagation across service boundaries

test "baggage creation and basic operations" {
  let baggage = Baggage::new()
  
  // Test that new baggage has no entries
  let missing_entry = Baggage::get_entry(baggage, "user.id")
  assert_eq(missing_entry, None)
}

test "baggage entry setting and getting" {
  let baggage = Baggage::new()
  
  // Set a single entry
  let updated_baggage = Baggage::set_entry(baggage, "user.id", "user123")
  
  // Get the entry
  let user_id = Baggage::get_entry(updated_baggage, "user.id")
  assert_eq(user_id, Some("user123"))
  
  // Test getting non-existent entry
  let missing_entry = Baggage::get_entry(updated_baggage, "session.id")
  assert_eq(missing_entry, None)
}

test "baggage with multiple entries" {
  let baggage = Baggage::new()
  
  // Set multiple entries
  let baggage1 = Baggage::set_entry(baggage, "user.id", "user123")
  let baggage2 = Baggage::set_entry(baggage1, "session.id", "session456")
  let baggage3 = Baggage::set_entry(baggage2, "request.id", "req789")
  
  // Verify all entries exist
  let user_id = Baggage::get_entry(baggage3, "user.id")
  assert_eq(user_id, Some("user123"))
  
  let session_id = Baggage::get_entry(baggage3, "session.id")
  assert_eq(session_id, Some("session456"))
  
  let request_id = Baggage::get_entry(baggage3, "request.id")
  assert_eq(request_id, Some("req789"))
}

test "baggage entry removal" {
  let baggage = Baggage::new()
  
  // Set multiple entries
  let baggage1 = Baggage::set_entry(baggage, "user.id", "user123")
  let baggage2 = Baggage::set_entry(baggage1, "session.id", "session456")
  let baggage3 = Baggage::set_entry(baggage2, "request.id", "req789")
  
  // Remove an entry
  let baggage_after_removal = Baggage::remove_entry(baggage3, "session.id")
  
  // Verify entry is removed
  let removed_entry = Baggage::get_entry(baggage_after_removal, "session.id")
  assert_eq(removed_entry, None)
  
  // Verify other entries still exist
  let user_id = Baggage::get_entry(baggage_after_removal, "user.id")
  assert_eq(user_id, Some("user123"))
  
  let request_id = Baggage::get_entry(baggage_after_removal, "request.id")
  assert_eq(request_id, Some("req789"))
}

test "baggage with user context information" {
  let baggage = Baggage::new()
  
  // Set user-related baggage
  let baggage1 = Baggage::set_entry(baggage, "user.id", "user123")
  let baggage2 = Baggage::set_entry(baggage1, "user.role", "admin")
  let baggage3 = Baggage::set_entry(baggage2, "user.tenant", "tenant456")
  let baggage4 = Baggage::set_entry(baggage3, "user.locale", "en-US")
  
  // Verify user context
  let user_id = Baggage::get_entry(baggage4, "user.id")
  assert_eq(user_id, Some("user123"))
  
  let user_role = Baggage::get_entry(baggage4, "user.role")
  assert_eq(user_role, Some("admin"))
  
  let user_tenant = Baggage::get_entry(baggage4, "user.tenant")
  assert_eq(user_tenant, Some("tenant456"))
  
  let user_locale = Baggage::get_entry(baggage4, "user.locale")
  assert_eq(user_locale, Some("en-US"))
}

test "baggage with request tracing information" {
  let baggage = Baggage::new()
  
  // Set request tracing baggage
  let baggage1 = Baggage::set_entry(baggage, "correlation.id", "corr-123-abc")
  let baggage2 = Baggage::set_entry(baggage1, "request.source", "web-frontend")
  let baggage3 = Baggage::set_entry(baggage2, "request.version", "v1.2.3")
  let baggage4 = Baggage::set_entry(baggage3, "request.priority", "high")
  
  // Verify request tracing context
  let correlation_id = Baggage::get_entry(baggage4, "correlation.id")
  assert_eq(correlation_id, Some("corr-123-abc"))
  
  let request_source = Baggage::get_entry(baggage4, "request.source")
  assert_eq(request_source, Some("web-frontend"))
  
  let request_version = Baggage::get_entry(baggage4, "request.version")
  assert_eq(request_version, Some("v1.2.3"))
  
  let request_priority = Baggage::get_entry(baggage4, "request.priority")
  assert_eq(request_priority, Some("high"))
}

test "baggage with business context" {
  let baggage = Baggage::new()
  
  // Set business context baggage
  let baggage1 = Baggage::set_entry(baggage, "business.transaction.id", "txn-789-xyz")
  let baggage2 = Baggage::set_entry(baggage1, "business.product.id", "prod-456")
  let baggage3 = Baggage::set_entry(baggage2, "business.customer.segment", "enterprise")
  let baggage4 = Baggage::set_entry(baggage3, "business.region", "us-west")
  
  // Verify business context
  let transaction_id = Baggage::get_entry(baggage4, "business.transaction.id")
  assert_eq(transaction_id, Some("txn-789-xyz"))
  
  let product_id = Baggage::get_entry(baggage4, "business.product.id")
  assert_eq(product_id, Some("prod-456"))
  
  let customer_segment = Baggage::get_entry(baggage4, "business.customer.segment")
  assert_eq(customer_segment, Some("enterprise"))
  
  let region = Baggage::get_entry(baggage4, "business.region")
  assert_eq(region, Some("us-west"))
}

test "baggage with technical context" {
  let baggage = Baggage::new()
  
  // Set technical context baggage
  let baggage1 = Baggage::set_entry(baggage, "tech.service.version", "2.1.0")
  let baggage2 = Baggage::set_entry(baggage1, "tech.deployment.id", "deploy-abc123")
  let baggage3 = Baggage::set_entry(baggage2, "tech.feature.flags", "featureA,featureB")
  let baggage4 = Baggage::set_entry(baggage3, "tech.experiment.id", "exp-456")
  
  // Verify technical context
  let service_version = Baggage::get_entry(baggage4, "tech.service.version")
  assert_eq(service_version, Some("2.1.0"))
  
  let deployment_id = Baggage::get_entry(baggage4, "tech.deployment.id")
  assert_eq(deployment_id, Some("deploy-abc123"))
  
  let feature_flags = Baggage::get_entry(baggage4, "tech.feature.flags")
  assert_eq(feature_flags, Some("featureA,featureB"))
  
  let experiment_id = Baggage::get_entry(baggage4, "tech.experiment.id")
  assert_eq(experiment_id, Some("exp-456"))
}

test "baggage with special characters and encoding" {
  let baggage = Baggage::new()
  
  // Set entries with special characters
  let baggage1 = Baggage::set_entry(baggage, "user.email", "user@example.com")
  let baggage2 = Baggage::set_entry(baggage1, "request.path", "/api/v1/users/123")
  let baggage3 = Baggage::set_entry(baggage2, "query.params", "sort=name&filter=active")
  let baggage4 = Baggage::set_entry(baggage3, "unicode.value", "遥测系统")
  
  // Verify special character handling
  let user_email = Baggage::get_entry(baggage4, "user.email")
  assert_eq(user_email, Some("user@example.com"))
  
  let request_path = Baggage::get_entry(baggage4, "request.path")
  assert_eq(request_path, Some("/api/v1/users/123"))
  
  let query_params = Baggage::get_entry(baggage4, "query.params")
  assert_eq(query_params, Some("sort=name&filter=active"))
  
  let unicode_value = Baggage::get_entry(baggage4, "unicode.value")
  assert_eq(unicode_value, Some("遥测系统"))
}

test "baggage with empty and edge case values" {
  let baggage = Baggage::new()
  
  // Set entries with edge case values
  let baggage1 = Baggage::set_entry(baggage, "empty.string", "")
  let baggage2 = Baggage::set_entry(baggage1, "zero.string", "0")
  let baggage3 = Baggage::set_entry(baggage2, "space.only", " ")
  let baggage4 = Baggage::set_entry(baggage3, "long.value", "this-is-a-very-long-value-that-might-be-used-in-some-edge-cases-to-test-how-the-baggage-system-handles-long-strings")
  
  // Verify edge case handling
  let empty_string = Baggage::get_entry(baggage4, "empty.string")
  assert_eq(empty_string, Some(""))
  
  let zero_string = Baggage::get_entry(baggage4, "zero.string")
  assert_eq(zero_string, Some("0"))
  
  let space_only = Baggage::get_entry(baggage4, "space.only")
  assert_eq(space_only, Some(" "))
  
  let long_value = Baggage::get_entry(baggage4, "long.value")
  assert_eq(long_value, Some("this-is-a-very-long-value-that-might-be-used-in-some-edge-cases-to-test-how-the-baggage-system-handles-long-strings"))
}