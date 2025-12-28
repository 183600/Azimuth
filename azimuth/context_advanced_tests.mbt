// Context高级测试用例

test "Context根上下文创建测试" {
  let root_ctx = Context::root()
  assert_true(root_ctx.data is None)
}

test "ContextKey创建和使用测试" {
  let key = ContextKey::new("test.key")
  assert_eq(key.key, "test.key")
  
  let root_ctx = Context::root()
  let ctx_with_value = Context::with_value(root_ctx, key, "test_value")
  
  let retrieved_value = Context::get(ctx_with_value, key)
  assert_true(retrieved_value is Some)
  match retrieved_value {
    Some(value) => assert_eq(value, "test_value")
    _ => assert_true(false)
  }
}

test "Context多层级值传递测试" {
  let key1 = ContextKey::new("level1.key")
  let key2 = ContextKey::new("level2.key")
  
  let root_ctx = Context::root()
  let ctx1 = Context::with_value(root_ctx, key1, "level1_value")
  let ctx2 = Context::with_value(ctx1, key2, "level2_value")
  
  // 第一层的值应该仍然存在
  let value1 = Context::get(ctx2, key1)
  assert_true(value1 is Some)
  match value1 {
    Some(v) => assert_eq(v, "level1_value")
    _ => assert_true(false)
  }
  
  // 第二层的值应该存在
  let value2 = Context::get(ctx2, key2)
  assert_true(value2 is Some)
  match value2 {
    Some(v) => assert_eq(v, "level2_value")
    _ => assert_true(false)
  }
}

test "Context不存在的键测试" {
  let key = ContextKey::new("nonexistent.key")
  let root_ctx = Context::root()
  
  let value = Context::get(root_ctx, key)
  assert_true(value is None)
}

test "Context键值覆盖测试" {
  let key = ContextKey::new("override.key")
  
  let root_ctx = Context::root()
  let ctx1 = Context::with_value(root_ctx, key, "original_value")
  let ctx2 = Context::with_value(ctx1, key, "overridden_value")
  
  let value = Context::get(ctx2, key)
  assert_true(value is Some)
  match value {
    Some(v) => assert_eq(v, "overridden_value")
    _ => assert_true(false)
  }
}