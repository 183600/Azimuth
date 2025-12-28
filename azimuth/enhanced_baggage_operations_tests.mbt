// Azimuth Telemetry System - Enhanced Baggage Operations Tests
// Comprehensive test suite for baggage functionality

test "baggage_creation_and_basic_operations" {
  let baggage = Baggage::new()
  
  // Test initial state
  assert_eq(baggage.entries.length(), 0)
  
  // Test setting entries
  let updated_baggage = Baggage::set_entry(baggage, "user.id", "12345")
  let user_id = Baggage::get_entry(updated_baggage, "user.id")
  assert_eq(user_id, Some("12345"))
  
  // Test missing entry
  let missing_entry = Baggage::get_entry(baggage, "missing.key")
  assert_eq(missing_entry, None)
}

test "baggage_multiple_entries_operations" {
  let baggage = Baggage::new()
  
  // Set multiple entries
  let baggage1 = Baggage::set_entry(baggage, "user.id", "12345")
  let baggage2 = Baggage::set_entry(baggage1, "session.id", "abcdef")
  let baggage3 = Baggage::set_entry(baggage2, "request.id", "request-123")
  
  // Test retrieval of multiple entries
  let user_id = Baggage::get_entry(baggage3, "user.id")
  let session_id = Baggage::get_entry(baggage3, "session.id")
  let request_id = Baggage::get_entry(baggage3, "request.id")
  
  assert_eq(user_id, Some("12345"))
  assert_eq(session_id, Some("abcdef"))
  assert_eq(request_id, Some("request-123"))
  
  // Test missing entry
  let missing = Baggage::get_entry(baggage3, "missing.key")
  assert_eq(missing, None)
}

test "baggage_overwrite_behavior" {
  let baggage = Baggage::new()
  let key = "overwrite.test"
  
  // Set initial value
  let baggage1 = Baggage::set_entry(baggage, key, "initial_value")
  let initial_val = Baggage::get_entry(baggage1, key)
  assert_eq(initial_val, Some("initial_value"))
  
  // Overwrite with new value
  let baggage2 = Baggage::set_entry(baggage1, key, "new_value")
  let new_val = Baggage::get_entry(baggage2, key)
  assert_eq(new_val, Some("new_value"))
  
  // Overwrite again
  let baggage3 = Baggage::set_entry(baggage2, key, "final_value")
  let final_val = Baggage::get_entry(baggage3, key)
  assert_eq(final_val, Some("final_value"))
}

test "baggage_remove_operations" {
  let baggage = Baggage::new()
  
  // Set multiple entries
  let baggage1 = Baggage::set_entry(baggage, "keep.me", "value1")
  let baggage2 = Baggage::set_entry(baggage1, "remove.me", "value2")
  let baggage3 = Baggage::set_entry(baggage2, "also.keep", "value3")
  
  // Verify all entries exist
  assert_eq(Baggage::get_entry(baggage3, "keep.me"), Some("value1"))
  assert_eq(Baggage::get_entry(baggage3, "remove.me"), Some("value2"))
  assert_eq(Baggage::get_entry(baggage3, "also.keep"), Some("value3"))
  
  // Remove entry
  let baggage4 = Baggage::remove_entry(baggage3, "remove.me")
  
  // Test removal
  assert_eq(Baggage::get_entry(baggage4, "keep.me"), Some("value1"))
  assert_eq(Baggage::get_entry(baggage4, "remove.me"), None)
  assert_eq(Baggage::get_entry(baggage4, "also.keep"), Some("value3"))
}

test "baggage_with_special_characters_in_keys" {
  let baggage = Baggage::new()
  
  // Test keys with different patterns
  let baggage1 = Baggage::set_entry(baggage, "dotted.key", "dotted_value")
  let baggage2 = Baggage::set_entry(baggage1, "hyphenated-key", "hyphenated_value")
  let baggage3 = Baggage::set_entry(baggage2, "underscore_key", "underscore_value")
  let baggage4 = Baggage::set_entry(baggage3, "namespace.subnamespace.key", "namespaced_value")
  
  // Test retrieval
  assert_eq(Baggage::get_entry(baggage4, "dotted.key"), Some("dotted_value"))
  assert_eq(Baggage::get_entry(baggage4, "hyphenated-key"), Some("hyphenated_value"))
  assert_eq(Baggage::get_entry(baggage4, "underscore_key"), Some("underscore_value"))
  assert_eq(Baggage::get_entry(baggage4, "namespace.subnamespace.key"), Some("namespaced_value"))
}

test "baggage_with_special_characters_in_values" {
  let baggage = Baggage::new()
  
  // Test values with special characters
  let baggage1 = Baggage::set_entry(baggage, "special.chars", "special!@#$%^&*()")
  let baggage2 = Baggage::set_entry(baggage1, "unicode.chars", "测试中文")
  let baggage3 = Baggage::set_entry(baggage2, "json.value", "{\"key\":\"value\"}")
  let baggage4 = Baggage::set_entry(baggage3, "url.encoded", "Hello%20World")
  let baggage5 = Baggage::set_entry(baggage4, "base64.value", "SGVsbG8gV29ybGQ=")
  
  // Test retrieval
  assert_eq(Baggage::get_entry(baggage5, "special.chars"), Some("special!@#$%^&*()"))
  assert_eq(Baggage::get_entry(baggage5, "unicode.chars"), Some("测试中文"))
  assert_eq(Baggage::get_entry(baggage5, "json.value"), Some("{\"key\":\"value\"}"))
  assert_eq(Baggage::get_entry(baggage5, "url.encoded"), Some("Hello%20World"))
  assert_eq(Baggage::get_entry(baggage5, "base64.value"), Some("SGVsbG8gV29ybGQ="))
}

test "baggage_with_empty_values" {
  let baggage = Baggage::new()
  
  // Test empty string value
  let baggage1 = Baggage::set_entry(baggage, "empty.value", "")
  let empty_val = Baggage::get_entry(baggage1, "empty.value")
  assert_eq(empty_val, Some(""))
  
  // Test with whitespace-only values
  let baggage2 = Baggage::set_entry(baggage1, "whitespace.value", "   ")
  let whitespace_val = Baggage::get_entry(baggage2, "whitespace.value")
  assert_eq(whitespace_val, Some("   "))
  
  // Test with tab and newline characters
  let baggage3 = Baggage::set_entry(baggage2, "control.chars", "value\twith\ncontrol")
  let control_val = Baggage::get_entry(baggage3, "control.chars")
  assert_eq(control_val, Some("value\twith\ncontrol"))
}

test "baggage_with_common_telemetry_entries" {
  let baggage = Baggage::new()
  
  // Test common telemetry baggage entries
  let baggage1 = Baggage::set_entry(baggage, "user.id", "user-12345")
  let baggage2 = Baggage::set_entry(baggage1, "session.id", "session-abcdef")
  let baggage3 = Baggage::set_entry(baggage2, "request.id", "req-67890")
  let baggage4 = Baggage::set_entry(baggage3, "correlation.id", "corr-11111")
  let baggage5 = Baggage::set_entry(baggage4, "tenant.id", "tenant-999")
  let baggage6 = Baggage::set_entry(baggage5, "trace.chain", "service-a->service-b->service-c")
  
  // Test retrieval of common telemetry entries
  assert_eq(Baggage::get_entry(baggage6, "user.id"), Some("user-12345"))
  assert_eq(Baggage::get_entry(baggage6, "session.id"), Some("session-abcdef"))
  assert_eq(Baggage::get_entry(baggage6, "request.id"), Some("req-67890"))
  assert_eq(Baggage::get_entry(baggage6, "correlation.id"), Some("corr-11111"))
  assert_eq(Baggage::get_entry(baggage6, "tenant.id"), Some("tenant-999"))
  assert_eq(Baggage::get_entry(baggage6, "trace.chain"), Some("service-a->service-b->service-c"))
}

test "baggage_with_numeric_values_as_strings" {
  let baggage = Baggage::new()
  
  // Test numeric values stored as strings
  let baggage1 = Baggage::set_entry(baggage, "port", "8080")
  let baggage2 = Baggage::set_entry(baggage1, "timeout", "30.5")
  let baggage3 = Baggage::set_entry(baggage2, "retry.count", "3")
  let baggage4 = Baggage::set_entry(baggage3, "version", "1.2.3")
  let baggage5 = Baggage::set_entry(baggage4, "priority", "5")
  
  // Test retrieval
  assert_eq(Baggage::get_entry(baggage5, "port"), Some("8080"))
  assert_eq(Baggage::get_entry(baggage5, "timeout"), Some("30.5"))
  assert_eq(Baggage::get_entry(baggage5, "retry.count"), Some("3"))
  assert_eq(Baggage::get_entry(baggage5, "version"), Some("1.2.3"))
  assert_eq(Baggage::get_entry(baggage5, "priority"), Some("5"))
}

test "baggage_complex_service_chain_scenario" {
  let baggage = Baggage::new()
  
  // Simulate a complex service chain scenario
  // Service A adds initial baggage
  let baggage_a = Baggage::set_entry(baggage, "service.a.entry", "from-service-a")
  let baggage_a1 = Baggage::set_entry(baggage_a, "user.id", "user-123")
  
  // Service B adds more baggage
  let baggage_b = Baggage::set_entry(baggage_a1, "service.b.entry", "from-service-b")
  let baggage_b1 = Baggage::set_entry(baggage_b, "session.id", "session-456")
  
  // Service C adds more baggage
  let baggage_c = Baggage::set_entry(baggage_b1, "service.c.entry", "from-service-c")
  let baggage_c1 = Baggage::set_entry(baggage_c, "request.id", "req-789")
  
  // Service D modifies existing baggage
  let baggage_d = Baggage::set_entry(baggage_c1, "user.id", "user-123-updated")
  let baggage_d1 = Baggage::set_entry(baggage_d, "service.d.entry", "from-service-d")
  
  // Test final state
  assert_eq(Baggage::get_entry(baggage_d1, "service.a.entry"), Some("from-service-a"))
  assert_eq(Baggage::get_entry(baggage_d1, "service.b.entry"), Some("from-service-b"))
  assert_eq(Baggage::get_entry(baggage_d1, "service.c.entry"), Some("from-service-c"))
  assert_eq(Baggage::get_entry(baggage_d1, "service.d.entry"), Some("from-service-d"))
  assert_eq(Baggage::get_entry(baggage_d1, "user.id"), Some("user-123-updated"))
  assert_eq(Baggage::get_entry(baggage_d1, "session.id"), Some("session-456"))
  assert_eq(Baggage::get_entry(baggage_d1, "request.id"), Some("req-789"))
}

test "baggage_remove_and_add_operations" {
  let baggage = Baggage::new()
  
  // Set initial entries
  let baggage1 = Baggage::set_entry(baggage, "temp.value", "should-be-removed")
  let baggage2 = Baggage::set_entry(baggage1, "keep.value", "should-be-kept")
  let baggage3 = Baggage::set_entry(baggage2, "another.temp", "also-remove")
  
  // Verify all entries exist
  assert_eq(Baggage::get_entry(baggage3, "temp.value"), Some("should-be-removed"))
  assert_eq(Baggage::get_entry(baggage3, "keep.value"), Some("should-be-kept"))
  assert_eq(Baggage::get_entry(baggage3, "another.temp"), Some("also-remove"))
  
  // Remove temporary entries
  let baggage4 = Baggage::remove_entry(baggage3, "temp.value")
  let baggage5 = Baggage::remove_entry(baggage4, "another.temp")
  
  // Add new entries
  let baggage6 = Baggage::set_entry(baggage5, "new.value", "newly-added")
  let baggage7 = Baggage::set_entry(baggage6, "another.new", "also-new")
  
  // Test final state
  assert_eq(Baggage::get_entry(baggage7, "temp.value"), None)
  assert_eq(Baggage::get_entry(baggage7, "keep.value"), Some("should-be-kept"))
  assert_eq(Baggage::get_entry(baggage7, "another.temp"), None)
  assert_eq(Baggage::get_entry(baggage7, "new.value"), Some("newly-added"))
  assert_eq(Baggage::get_entry(baggage7, "another.new"), Some("also-new"))
}