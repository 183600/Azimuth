test "Context advanced operations test" {
  // Test context creation and basic operations
  let root_ctx = Context::root()
  @assertion.assert_eq(root_ctx.data, None)?
  
  // Test context with single value
  let user_key = ContextKey::new("user.id")
  let ctx_with_user = Context::with_value(root_ctx, user_key, "user-123")
  let user_id = Context::get(ctx_with_user, user_key)
  @assertion.assert_eq(user_id?, "user-123")?
  
  // Test context with multiple values
  let trace_key = ContextKey::new("trace.id")
  let session_key = ContextKey::new("session.id")
  let ctx_with_multiple = Context::with_value(
    Context::with_value(
      Context::with_value(root_ctx, user_key, "user-123"),
      trace_key, "trace-456"
    ),
    session_key, "session-789"
  )
  
  let trace_id = Context::get(ctx_with_multiple, trace_key)
  @assertion.assert_eq(trace_id?, "trace-456")?
  
  let session_id = Context::get(ctx_with_multiple, session_key)
  @assertion.assert_eq(session_id?, "session-789")?
  
  // Test context key isolation
  let different_user_key = ContextKey::new("user.id")
  let same_user_id = Context::get(ctx_with_multiple, different_user_key)
  @assertion.assert_eq(same_user_id?, "user-123")?
  
  // Test non-existent key
  let non_existent_key = ContextKey::new("non.existent")
  let non_existent_value = Context::get(ctx_with_multiple, non_existent_key)
  @assertion.assert_eq(non_existent_value, None)?
  
  // Test context override behavior
  let ctx_with_override = Context::with_value(ctx_with_multiple, user_key, "user-999")
  let overridden_user_id = Context::get(ctx_with_override, user_key)
  @assertion.assert_eq(overridden_user_id?, "user-999")?
  
  // Verify other values are preserved (simplified implementation)
  let preserved_trace_id = Context::get(ctx_with_override, trace_key)
  @assertion.assert_eq(preserved_trace_id?, "trace-456")?
}

test "Context with complex data types test" {
  // Test context with complex string values
  let root_ctx = Context::root()
  
  // JSON-like string data
  let json_key = ContextKey::new("user.data")
  let json_data = "{\"id\":\"123\",\"name\":\"Test User\",\"roles\":[\"admin\",\"user\"]}"
  let ctx_with_json = Context::with_value(root_ctx, json_key, json_data)
  let retrieved_json = Context::get(ctx_with_json, json_key)
  @assertion.assert_eq(retrieved_json?, json_data)?
  
  // Configuration data
  let config_key = ContextKey::new("service.config")
  let config_data = "timeout=30,retries=3,backoff=exponential"
  let ctx_with_config = Context::with_value(ctx_with_json, config_key, config_data)
  let retrieved_config = Context::get(ctx_with_config, config_key)
  @assertion.assert_eq(retrieved_config?, config_data)?
  
  // Long string data
  let long_key = ContextKey::new("large.payload")
  let long_data = "This is a very long string that might be used to store large amounts of context data such as serialized objects, base64 encoded binary data, or extensive logging information that needs to be passed through the context chain."
  let ctx_with_long = Context::with_value(ctx_with_config, long_key, long_data)
  let retrieved_long = Context::get(ctx_with_long, long_key)
  @assertion.assert_eq(retrieved_long?, long_data)?
  
  // Special characters and encoding
  let special_key = ContextKey::new("special.chars")
  let special_data = "Special chars: àáâãäåæçèéêë ñòóôõö ùúûüý ÿ 中文 العربية русский"
  let ctx_with_special = Context::with_value(ctx_with_long, special_key, special_data)
  let retrieved_special = Context::get(ctx_with_special, special_key)
  @assertion.assert_eq(retrieved_special?, special_data)?
  
  // Verify all data is accessible
  @assertion.assert_eq(Context::get(ctx_with_special, json_key)?, json_data)?
  @assertion.assert_eq(Context::get(ctx_with_special, config_key)?, config_data)?
  @assertion.assert_eq(Context::get(ctx_with_special, long_key)?, long_data)?
  @assertion.assert_eq(Context::get(ctx_with_special, special_key)?, special_data)?
}

test "Context key naming and validation test" {
  let root_ctx = Context::root()
  
  // Test various key naming patterns
  let keys_and_values = [
    ("simple.key", "simple.value"),
    ("with.dots.in.name", "dotted.value"),
    ("with_underscores", "underscored.value"),
    ("with-numbers123", "numeric.value"),
    ("UPPERCASE.KEY", "uppercase.value"),
    ("MixedCase.Key", "mixed.value"),
    ("key.with.many.dots.and.subkeys", "deep.value"),
    ("123.starting.with.numbers", "numeric.start.value"),
    ("key-with-dashes", "dashed.value")
  ]
  
  let mut ctx = root_ctx
  for (key_name, value) in keys_and_values {
    let key = ContextKey::new(key_name)
    ctx = Context::with_value(ctx, key, value)
    let retrieved = Context::get(ctx, key)
    @assertion.assert_eq(retrieved?, value)?
  }
  
  // Test empty string key (edge case)
  let empty_key = ContextKey::new("")
  let ctx_with_empty = Context::with_value(ctx, empty_key, "empty.key.value")
  let empty_value = Context::get(ctx_with_empty, empty_key)
  @assertion.assert_eq(empty_value?, "empty.key.value")?
  
  // Test very long key name
  let long_key_name = "this.is.a.very.long.key.name.that.might.be.used.in.some.enterprise.applications.to.store.very.specific.context.information.with.hierarchical.naming.conventions"
  let long_key = ContextKey::new(long_key_name)
  let ctx_with_long_key = Context::with_value(ctx_with_empty, long_key, "long.key.value")
  let long_key_value = Context::get(ctx_with_long_key, long_key)
  @assertion.assert_eq(long_key_value?, "long.key.value")?
}