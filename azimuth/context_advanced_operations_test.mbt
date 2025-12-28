// Context Advanced Operations Test Suite
// Tests for Context management, ContextKey operations, and context propagation

test "context_root_creation" {
  // Test creating a root context
  let root_ctx = Context::root()
  match root_ctx.data {
    None => assert_true(true)
    Some(_) => assert_true(false, "Root context should have no data")
  }
}

test "context_key_creation_and_usage" {
  // Test ContextKey creation and usage
  let user_key = ContextKey::new("user.id")
  let trace_key = ContextKey::new("trace.id")
  let request_key = ContextKey::new("request.id")
  
  assert_eq(user_key.key, "user.id")
  assert_eq(trace_key.key, "trace.id")
  assert_eq(request_key.key, "request.id")
}

test "context_with_value_operations" {
  // Test setting values in context
  let root_ctx = Context::root()
  let user_key = ContextKey::new("user.id")
  
  // Set a value
  let ctx_with_user = Context::with_value(root_ctx, user_key, "user123")
  
  // Verify the value is stored
  match ctx_with_user.data {
    Some((key, value)) => {
      assert_eq(key, "user.id")
      assert_eq(value, "user123")
    }
    None => assert_true(false, "Context should have data")
  }
}

test "context_value_retrieval" {
  // Test retrieving values from context
  let root_ctx = Context::root()
  let user_key = ContextKey::new("user.id")
  let session_key = ContextKey::new("session.id")
  
  // Set user value
  let ctx_with_user = Context::with_value(root_ctx, user_key, "user123")
  
  // Retrieve existing value
  let retrieved_user = Context::get(ctx_with_user, user_key)
  match retrieved_user {
    Some(value) => assert_eq(value, "user123")
    None => assert_true(false, "Should retrieve user value")
  }
  
  // Try to retrieve non-existing value
  let retrieved_session = Context::get(ctx_with_user, session_key)
  match retrieved_session {
    Some(_) => assert_true(false, "Should not find session value")
    None => assert_true(true) // Expected
  }
}

test "context_multiple_values_isolation" {
  // Test that different context keys maintain isolation
  let root_ctx = Context::root()
  let user_key = ContextKey::new("user.id")
  let role_key = ContextKey::new("user.role")
  let trace_key = ContextKey::new("trace.id")
  
  // Create contexts with different values
  let ctx1 = Context::with_value(root_ctx, user_key, "user123")
  let ctx2 = Context::with_value(ctx1, role_key, "admin")
  let ctx3 = Context::with_value(ctx2, trace_key, "trace456")
  
  // Each key should retrieve its own value
  let user_val = Context::get(ctx3, user_key)
  let role_val = Context::get(ctx3, role_key)
  let trace_val = Context::get(ctx3, trace_key)
  
  match user_val {
    Some(value) => assert_eq(value, "user123")
    None => assert_true(false, "Should find user value")
  }
  
  match role_val {
    Some(value) => assert_eq(value, "admin")
    None => assert_true(false, "Should find role value")
  }
  
  match trace_val {
    Some(value) => assert_eq(value, "trace456")
    None => assert_true(false, "Should find trace value")
  }
}

test "context_key_type_safety" {
  // Test that ContextKey maintains type safety
  let string_key = ContextKey::new("string.key")
  
  let root_ctx = Context::root()
  let ctx_with_string = Context::with_value(root_ctx, string_key, "string_value")
  
  // Should be able to retrieve the string value
  let retrieved = Context::get(ctx_with_string, string_key)
  match retrieved {
    Some(value) => {
      assert_eq(value, "string_value")
      // Verify it's actually a string
      assert_eq(value.length(), 13)
    }
    None => assert_true(false, "Should retrieve string value")
  }
}

test "context_empty_value_handling" {
  // Test handling of empty string values
  let root_ctx = Context::root()
  let empty_key = ContextKey::new("empty.key")
  
  let ctx_with_empty = Context::with_value(root_ctx, empty_key, "")
  
  let retrieved = Context::get(ctx_with_empty, empty_key)
  match retrieved {
    Some(value) => {
      assert_eq(value, "")
      assert_eq(value.length(), 0)
    }
    None => assert_true(false, "Should retrieve empty string")
  }
}

test "context_special_characters_in_keys" {
  // Test context keys with special characters
  let special_key = ContextKey::new("user.email@domain.com")
  let unicode_key = ContextKey::new("用户.标识")
  let dot_key = ContextKey::new("nested.key.path")
  
  let root_ctx = Context::root()
  
  let ctx1 = Context::with_value(root_ctx, special_key, "test@example.com")
  let ctx2 = Context::with_value(ctx1, unicode_key, "用户123")
  let ctx3 = Context::with_value(ctx2, dot_key, "nested_value")
  
  // Test retrieval of special character keys
  let email_val = Context::get(ctx3, special_key)
  match email_val {
    Some(value) => assert_eq(value, "test@example.com")
    None => assert_true(false, "Should retrieve email value")
  }
  
  let unicode_val = Context::get(ctx3, unicode_key)
  match unicode_val {
    Some(value) => assert_eq(value, "用户123")
    None => assert_true(false, "Should retrieve unicode value")
  }
  
  let nested_val = Context::get(ctx3, dot_key)
  match nested_val {
    Some(value) => assert_eq(value, "nested_value")
    None => assert_true(false, "Should retrieve nested value")
  }
}

test "context_long_values_handling" {
  // Test handling of long string values
  let root_ctx = Context::root()
  let long_key = ContextKey::new("long.value")
  
  let long_value = "This is a very long string that might be used to store " + 
                   "complex information such as JSON payloads, detailed error " +
                   "messages, or other verbose data that needs to be passed " +
                   "through the context system."
  
  let ctx_with_long = Context::with_value(root_ctx, long_key, long_value)
  
  let retrieved = Context::get(ctx_with_long, long_key)
  match retrieved {
    Some(value) => {
      assert_eq(value, long_value)
      assert_true(value.length() > 100)
    }
    None => assert_true(false, "Should retrieve long value")
  }
}

test "context_key_case_sensitivity" {
  // Test that context keys are case sensitive
  let root_ctx = Context::root()
  let lower_key = ContextKey::new("test.key")
  let upper_key = ContextKey::new("TEST.KEY")
  let mixed_key = ContextKey::new("Test.Key")
  
  let ctx1 = Context::with_value(root_ctx, lower_key, "lower_value")
  let ctx2 = Context::with_value(ctx1, upper_key, "upper_value")
  let ctx3 = Context::with_value(ctx2, mixed_key, "mixed_value")
  
  // Each key should be distinct
  let lower_val = Context::get(ctx3, lower_key)
  let upper_val = Context::get(ctx3, upper_key)
  let mixed_val = Context::get(ctx3, mixed_key)
  
  match lower_val {
    Some(value) => assert_eq(value, "lower_value")
    None => assert_true(false, "Should find lower case key")
  }
  
  match upper_val {
    Some(value) => assert_eq(value, "upper_value")
    None => assert_true(false, "Should find upper case key")
  }
  
  match mixed_val {
    Some(value) => assert_eq(value, "mixed_value")
    None => assert_true(false, "Should find mixed case key")
  }
}