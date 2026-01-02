// Azimuth Baggage Cross-Context Propagation Tests
// Test baggage creation, entry management, and context propagation

test "baggage_creation_and_initial_state" {
  let baggage = Baggage::new()
  
  // Test initial state - should have no entries
  assert_eq!(baggage.entries.length, 0)
  
  // Test getting non-existent entry
  match Baggage::get_entry(baggage, "non_existent") {
    Some(_) => @test.abort("Expected None for non-existent entry")
    None => @test.succeed()
  }
}

test "baggage_set_and_get_entry" {
  let baggage = Baggage::new()
  
  // Set a single entry
  let updated_baggage = Baggage::set_entry(baggage, "user.id", "12345")
  
  // Get the entry
  match Baggage::get_entry(updated_baggage, "user.id") {
    Some(value) => assert_eq!(value, "12345")
    None => @test.abort("Expected to find user.id entry")
  }
  
  // Test non-existent entry
  match Baggage::get_entry(updated_baggage, "non.existent") {
    Some(_) => @test.abort("Expected None for non-existent entry")
    None => @test.succeed()
  }
}

test "baggage_multiple_entries" {
  let baggage = Baggage::new()
  
  // Set multiple entries
  let baggage1 = Baggage::set_entry(baggage, "user.id", "12345")
  let baggage2 = Baggage::set_entry(baggage1, "request.id", "req-67890")
  let baggage3 = Baggage::set_entry(baggage2, "trace.source", "frontend")
  
  // Verify all entries exist
  match Baggage::get_entry(baggage3, "user.id") {
    Some(value) => assert_eq!(value, "12345")
    None => @test.abort("Expected to find user.id entry")
  }
  
  match Baggage::get_entry(baggage3, "request.id") {
    Some(value) => assert_eq!(value, "req-67890")
    None => @test.abort("Expected to find request.id entry")
  }
  
  match Baggage::get_entry(baggage3, "trace.source") {
    Some(value) => assert_eq!(value, "frontend")
    None => @test.abort("Expected to find trace.source entry")
  }
}

test "baggage_entry_overwrite" {
  let baggage = Baggage::new()
  
  // Set initial value
  let baggage1 = Baggage::set_entry(baggage, "user.id", "12345")
  
  // Verify initial value
  match Baggage::get_entry(baggage1, "user.id") {
    Some(value) => assert_eq!(value, "12345")
    None => @test.abort("Expected to find user.id entry")
  }
  
  // Overwrite with new value
  let baggage2 = Baggage::set_entry(baggage1, "user.id", "67890")
  
  // Verify new value
  match Baggage::get_entry(baggage2, "user.id") {
    Some(value) => assert_eq!(value, "67890")
    None => @test.abort("Expected to find updated user.id entry")
  }
}

test "baggage_remove_entry" {
  let baggage = Baggage::new()
  
  // Set multiple entries
  let baggage1 = Baggage::set_entry(baggage, "user.id", "12345")
  let baggage2 = Baggage::set_entry(baggage1, "request.id", "req-67890")
  
  // Verify entries exist
  match Baggage::get_entry(baggage2, "user.id") {
    Some(_) => @test.succeed()
    None => @test.abort("Expected to find user.id entry")
  }
  
  match Baggage::get_entry(baggage2, "request.id") {
    Some(_) => @test.succeed()
    None => @test.abort("Expected to find request.id entry")
  }
  
  // Remove one entry
  let baggage3 = Baggage::remove_entry(baggage2, "user.id")
  
  // Verify removal
  match Baggage::get_entry(baggage3, "user.id") {
    Some(_) => @test.abort("Expected user.id to be removed")
    None => @test.succeed()
  }
  
  // Verify other entry still exists
  match Baggage::get_entry(baggage3, "request.id") {
    Some(_) => @test.succeed()
    None => @test.abort("Expected request.id to still exist")
  }
}

test "baggage_remove_nonexistent_entry" {
  let baggage = Baggage::new()
  
  // Set an entry
  let baggage1 = Baggage::set_entry(baggage, "existing.key", "value")
  
  // Try to remove non-existent entry
  let baggage2 = Baggage::remove_entry(baggage1, "non.existent")
  
  // Verify original entry still exists
  match Baggage::get_entry(baggage2, "existing.key") {
    Some(value) => assert_eq!(value, "value")
    None => @test.abort("Expected existing.key to still exist")
  }
}

test "baggage_special_characters_in_keys_and_values" {
  let baggage = Baggage::new()
  
  // Test special characters in keys and values
  let baggage1 = Baggage::set_entry(baggage, "special-key!@#", "special-value#$%")
  let baggage2 = Baggage::set_entry(baggage1, "unicode.键", "unicode.值")
  let baggage3 = Baggage::set_entry(baggage2, "space key", "space value")
  
  // Verify special character entries
  match Baggage::get_entry(baggage3, "special-key!@#") {
    Some(value) => assert_eq!(value, "special-value#$%")
    None => @test.abort("Expected to find special character key")
  }
  
  match Baggage::get_entry(baggage3, "unicode.键") {
    Some(value) => assert_eq!(value, "unicode.值")
    None => @test.abort("Expected to find unicode key")
  }
  
  match Baggage::get_entry(baggage3, "space key") {
    Some(value) => assert_eq!(value, "space value")
    None => @test.abort("Expected to find space key")
  }
}

test "baggage_empty_and_empty_string_values" {
  let baggage = Baggage::new()
  
  // Test empty string values
  let baggage1 = Baggage::set_entry(baggage, "empty.value", "")
  
  match Baggage::get_entry(baggage1, "empty.value") {
    Some(value) => assert_eq!(value, "")
    None => @test.abort("Expected to find empty string value")
  }
  
  // Test normal value alongside empty
  let baggage2 = Baggage::set_entry(baggage1, "normal.value", "normal")
  
  match Baggage::get_entry(baggage2, "empty.value") {
    Some(value) => assert_eq!(value, "")
    None => @test.abort("Expected empty value to still exist")
  }
  
  match Baggage::get_entry(baggage2, "normal.value") {
    Some(value) => assert_eq!(value, "normal")
    None => @test.abort("Expected normal value to exist")
  }
}

test "baggage_case_sensitivity" {
  let baggage = Baggage::new()
  
  // Set entries with different cases
  let baggage1 = Baggage::set_entry(baggage, "CaseSensitive", "value1")
  let baggage2 = Baggage::set_entry(baggage1, "casesensitive", "value2")
  let baggage3 = Baggage::set_entry(baggage2, "CASESENSITIVE", "value3")
  
  // Verify case sensitivity
  match Baggage::get_entry(baggage3, "CaseSensitive") {
    Some(value) => assert_eq!(value, "value1")
    None => @test.abort("Expected to find CaseSensitive")
  }
  
  match Baggage::get_entry(baggage3, "casesensitive") {
    Some(value) => assert_eq!(value, "value2")
    None => @test.abort("Expected to find casesensitive")
  }
  
  match Baggage::get_entry(baggage3, "CASESENSITIVE") {
    Some(value) => assert_eq!(value, "value3")
    None => @test.abort("Expected to find CASESENSITIVE")
  }
}

test "baggage_long_values" {
  let baggage = Baggage::new()
  
  // Test with very long values
  let long_value = "a".repeat(1000)
  let baggage1 = Baggage::set_entry(baggage, "long.key", long_value)
  
  match Baggage::get_entry(baggage1, "long.key") {
    Some(value) => assert_eq!(value.length, 1000)
    None => @test.abort("Expected to find long value")
  }
  
  // Test with long keys
  let long_key = "k".repeat(100)
  let baggage2 = Baggage::set_entry(baggage1, long_key, "short_value")
  
  match Baggage::get_entry(baggage2, long_key) {
    Some(value) => assert_eq!(value, "short_value")
    None => @test.abort("Expected to find entry with long key")
  }
}

test "baggage_complex_workflow" {
  let baggage = Baggage::new()
  
  // Simulate complex baggage workflow
  let baggage1 = Baggage::set_entry(baggage, "user.id", "user123")
  let baggage2 = Baggage::set_entry(baggage1, "session.id", "session456")
  let baggage3 = Baggage::set_entry(baggage2, "request.id", "req789")
  
  // Simulate request processing
  let baggage4 = Baggage::set_entry(baggage3, "processing.stage", "validation")
  let baggage5 = Baggage::set_entry(baggage4, "processing.attempt", "1")
  
  // Update processing stage
  let baggage6 = Baggage::set_entry(baggage5, "processing.stage", "execution")
  let baggage7 = Baggage::set_entry(baggage6, "processing.attempt", "2")
  
  // Remove temporary entries
  let baggage8 = Baggage::remove_entry(baggage7, "processing.attempt")
  
  // Add final entries
  let final_baggage = Baggage::set_entry(baggage8, "result.status", "success")
  
  // Verify final state
  match Baggage::get_entry(final_baggage, "user.id") {
    Some(value) => assert_eq!(value, "user123")
    None => @test.abort("Expected user.id to persist")
  }
  
  match Baggage::get_entry(final_baggage, "session.id") {
    Some(value) => assert_eq!(value, "session456")
    None => @test.abort("Expected session.id to persist")
  }
  
  match Baggage::get_entry(final_baggage, "processing.stage") {
    Some(value) => assert_eq!(value, "execution")
    None => @test.abort("Expected processing.stage to be updated")
  }
  
  match Baggage::get_entry(final_baggage, "processing.attempt") {
    Some(_) => @test.abort("Expected processing.attempt to be removed")
    None => @test.succeed()
  }
  
  match Baggage::get_entry(final_baggage, "result.status") {
    Some(value) => assert_eq!(value, "success")
    None => @test.abort("Expected result.status to be set")
  }
}

test "baggage_context_integration" {
  let baggage = Baggage::new()
  
  // Create baggage with context-related entries
  let baggage1 = Baggage::set_entry(baggage, "correlation.id", "corr-12345")
  let baggage2 = Baggage::set_entry(baggage1, "trace.id", "trace-67890")
  let baggage3 = Baggage::set_entry(baggage2, "span.id", "span-abcde")
  
  // Create context with baggage
  let context_key = ContextKey::new("baggage")
  let context = Context::with_value(Context::root(), context_key, "serialized_baggage")
  
  // Verify context integration
  match Context::get(context, context_key) {
    Some(value) => assert_eq!(value, "serialized_baggage")
    None => @test.abort("Expected to find baggage in context")
  }
  
  // Verify baggage entries still exist
  match Baggage::get_entry(baggage3, "correlation.id") {
    Some(value) => assert_eq!(value, "corr-12345")
    None => @test.abort("Expected correlation.id to exist")
  }
}