// Baggage Complex Operations Test Suite for Azimuth Telemetry System
// Tests advanced baggage operations including multiple entries, cascading updates, and removal

test "baggage multiple entries management" {
  let baggage = Baggage::new()
  
  // Add multiple entries
  let updated1 = Baggage::set_entry(baggage, "user.id", "user-123")
  let updated2 = Baggage::set_entry(updated1, "request.id", "req-456")
  let updated3 = Baggage::set_entry(updated2, "session.id", "session-789")
  
  // Verify all entries exist
  assert_eq(Baggage::get_entry(updated3, "user.id"), Some("user-123"))
  assert_eq(Baggage::get_entry(updated3, "request.id"), Some("req-456"))
  assert_eq(Baggage::get_entry(updated3, "session.id"), Some("session-789"))
  
  // Verify non-existent entry
  assert_eq(Baggage::get_entry(updated3, "missing.key"), None)
}

test "baggage entry updates and overrides" {
  let baggage = Baggage::new()
  
  // Set initial value
  let with_value = Baggage::set_entry(baggage, "environment", "development")
  assert_eq(Baggage::get_entry(with_value, "environment"), Some("development"))
  
  // Update the same key
  let updated = Baggage::set_entry(with_value, "environment", "production")
  assert_eq(Baggage::get_entry(updated, "environment"), Some("production"))
  
  // Add another entry
  let with_another = Baggage::set_entry(updated, "service.version", "2.1.0")
  assert_eq(Baggage::get_entry(with_another, "environment"), Some("production"))
  assert_eq(Baggage::get_entry(with_another, "service.version"), Some("2.1.0"))
}

test "baggage removal operations" {
  let baggage = Baggage::new()
  
  // Add multiple entries
  let with_entries = Baggage::set_entry(
    Baggage::set_entry(
      Baggage::set_entry(baggage, "key1", "value1"),
      "key2", "value2"
    ),
    "key3", "value3"
  )
  
  // Verify all entries exist
  assert_eq(Baggage::get_entry(with_entries, "key1"), Some("value1"))
  assert_eq(Baggage::get_entry(with_entries, "key2"), Some("value2"))
  assert_eq(Baggage::get_entry(with_entries, "key3"), Some("value3"))
  
  // Remove one entry
  let without_key2 = Baggage::remove_entry(with_entries, "key2")
  assert_eq(Baggage::get_entry(without_key2, "key1"), Some("value1"))
  assert_eq(Baggage::get_entry(without_key2, "key2"), None)
  assert_eq(Baggage::get_entry(without_key2, "key3"), Some("value3"))
  
  // Try to remove non-existent entry
  let still_same = Baggage::remove_entry(without_key2, "nonexistent")
  assert_eq(Baggage::get_entry(still_same, "key1"), Some("value1"))
  assert_eq(Baggage::get_entry(still_same, "key2"), None)
  assert_eq(Baggage::get_entry(still_same, "key3"), Some("value3"))
}

test "baggage with special characters and values" {
  let baggage = Baggage::new()
  
  // Test keys with special characters
  let special_keys = Baggage::set_entry(baggage, "service.name", "payment-service")
  assert_eq(Baggage::get_entry(special_keys, "service.name"), Some("payment-service"))
  
  let with_underscores = Baggage::set_entry(special_keys, "user_id", "12345")
  assert_eq(Baggage::get_entry(with_underscores, "user_id"), Some("12345"))
  
  let with_hyphens = Baggage::set_entry(with_underscores, "http-status-code", "200")
  assert_eq(Baggage::get_entry(with_hyphens, "http-status-code"), Some("200"))
  
  // Test values with special characters
  let with_url = Baggage::set_entry(with_hyphens, "callback.url", "https://example.com/callback?token=abc123")
  assert_eq(Baggage::get_entry(with_url, "callback.url"), Some("https://example.com/callback?token=abc123"))
  
  let with_json = Baggage::set_entry(with_url, "metadata", "{\"type\":\"user\",\"action\":\"login\"}")
  assert_eq(Baggage::get_entry(with_json, "metadata"), Some("{\"type\":\"user\",\"action\":\"login\"}"))
  
  // Test empty values
  let with_empty = Baggage::set_entry(with_json, "empty.value", "")
  assert_eq(Baggage::get_entry(with_empty, "empty.value"), Some(""))
}

test "baggage cascading operations" {
  let baggage = Baggage::new()
  
  // Build up baggage through cascading operations
  let cascade1 = Baggage::set_entry(baggage, "trace.id", "trace-123")
  let cascade2 = Baggage::set_entry(cascade1, "span.id", "span-456")
  let cascade3 = Baggage::set_entry(cascade2, "user.id", "user-789")
  let cascade4 = Baggage::set_entry(cascade3, "operation", "process_payment")
  
  // Verify all values are preserved through cascading
  assert_eq(Baggage::get_entry(cascade4, "trace.id"), Some("trace-123"))
  assert_eq(Baggage::get_entry(cascade4, "span.id"), Some("span-456"))
  assert_eq(Baggage::get_entry(cascade4, "user.id"), Some("user-789"))
  assert_eq(Baggage::get_entry(cascade4, "operation"), Some("process_payment"))
  
  // Remove through cascading
  let cascade_removed1 = Baggage::remove_entry(cascade4, "span.id")
  assert_eq(Baggage::get_entry(cascade_removed1, "trace.id"), Some("trace-123"))
  assert_eq(Baggage::get_entry(cascade_removed1, "span.id"), None)
  assert_eq(Baggage::get_entry(cascade_removed1, "user.id"), Some("user-789"))
  assert_eq(Baggage::get_entry(cascade_removed1, "operation"), Some("process_payment"))
}