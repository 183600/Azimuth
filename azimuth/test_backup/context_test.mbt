// Test file for Context and ContextKey functionality

pub test "context creation and basic operations" {
  let root_ctx = Context::root()
  assert_eq(root_ctx.data, None)
  
  let key = ContextKey::new("test.key")
  assert_eq(key.key, "test.key")
  
  let ctx_with_value = Context::with_value(root_ctx, key, "test.value")
  assert_eq(ctx_with_value.data, Some(("test.key", "test.value")))
  
  let retrieved_value = Context::get(ctx_with_value, key)
  assert_eq(retrieved_value, Some("test.value"))
}

pub test "context key operations" {
  let key1 = ContextKey::new("key1")
  let key2 = ContextKey::new("key2")
  let key3 = ContextKey::new("key1")  // Same as key1
  
  assert_eq(key1.key, "key1")
  assert_eq(key2.key, "key2")
  assert_eq(key3.key, "key1")
  
  let ctx = Context::root()
  let ctx1 = Context::with_value(ctx, key1, "value1")
  let ctx2 = Context::with_value(ctx1, key2, "value2")
  
  // Test retrieval with different keys
  assert_eq(Context::get(ctx2, key1), Some("value1"))
  assert_eq(Context::get(ctx2, key2), Some("value2"))
  assert_eq(Context::get(ctx2, key3), Some("value1"))  // Same key as key1
  
  // Test non-existent key
  let non_existent_key = ContextKey::new("nonexistent")
  assert_eq(Context::get(ctx2, non_existent_key), None)
}

pub test "context chaining and override" {
  let ctx = Context::root()
  let key = ContextKey::new("override.key")
  
  let ctx1 = Context::with_value(ctx, key, "first.value")
  assert_eq(Context::get(ctx1, key), Some("first.value"))
  
  let ctx2 = Context::with_value(ctx1, key, "second.value")
  assert_eq(Context::get(ctx2, key), Some("second.value"))
  
  // Test that original context is not affected
  assert_eq(Context::get(ctx1, key), Some("first.value"))
}

pub test "context with special characters" {
  let ctx = Context::root()
  
  let special_key = ContextKey::new("special.key.with.dots")
  let ctx_special = Context::with_value(ctx, special_key, "special.value")
  assert_eq(Context::get(ctx_special, special_key), Some("special.value"))
  
  let unicode_key = ContextKey::new("测试键")
  let ctx_unicode = Context::with_value(ctx_special, unicode_key, "测试值")
  assert_eq(Context::get(ctx_unicode, unicode_key), Some("测试值"))
  
  let empty_key = ContextKey::new("")
  let ctx_empty = Context::with_value(ctx_unicode, empty_key, "empty.key.value")
  assert_eq(Context::get(ctx_empty, empty_key), Some("empty.key.value"))
}

pub test "context edge cases" {
  let ctx = Context::root()
  
  // Test with very long key
  let long_key_str = "a".repeat(100)
  let long_key = ContextKey::new(long_key_str)
  let ctx_long = Context::with_value(ctx, long_key, "long.key.value")
  assert_eq(Context::get(ctx_long, long_key), Some("long.key.value"))
  
  // Test with empty value
  let empty_value_key = ContextKey::new("empty.value")
  let ctx_empty_val = Context::with_value(ctx_long, empty_value_key, "")
  assert_eq(Context::get(ctx_empty_val, empty_value_key), Some(""))
  
  // Test with whitespace value
  let whitespace_key = ContextKey::new("whitespace")
  let ctx_whitespace = Context::with_value(ctx_empty_val, whitespace_key, "   ")
  assert_eq(Context::get(ctx_whitespace, whitespace_key), Some("   "))
}