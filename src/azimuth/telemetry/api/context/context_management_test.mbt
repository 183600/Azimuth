// Context模块上下文管理测试
// 测试上下文的创建、值存储和检索功能

test "context_empty_creation" {
  let ctx = Context::empty()
  assert_eq(ctx.values.length(), 0)
}

test "context_with_value_storage" {
  let ctx = Context::empty()
  let key = create_key("user_id")
  let ctx_with_value = ctx.with_value(key, "12345")
  
  assert_eq(ctx_with_value.values.length(), 1)
  match ctx_with_value.values[0] {
    (k, v) => {
      assert_eq(k, "user_id")
      assert_eq(v, "12345")
    }
    _ => @test.fail("Expected key-value pair")
  }
}

test "context_value_retrieval" {
  let ctx = Context::empty()
  let user_id_key = create_key("user_id")
  let session_key = create_key("session_id")
  
  let ctx_with_values = ctx
    .with_value(user_id_key, "user123")
    .with_value(session_key, "session456")
  
  match ctx_with_values.get(user_id_key) {
    Some(value) => assert_eq(value, "user123")
    None => @test.fail("Expected user_id value")
  }
  
  match ctx_with_values.get(session_key) {
    Some(value) => assert_eq(value, "session456")
    None => @test.fail("Expected session_id value")
  }
}

test "context_get_nonexistent_key" {
  let ctx = Context::empty()
  let nonexistent_key = create_key("nonexistent")
  
  match ctx.get(nonexistent_key) {
    Some(_) => @test.fail("Expected None for nonexistent key")
    None => @test.succeed()
  }
}

test "context_multiple_values_order_preservation" {
  let ctx = Context::empty()
  let key1 = create_key("first")
  let key2 = create_key("second")
  let key3 = create_key("third")
  
  let ctx_with_values = ctx
    .with_value(key1, "value1")
    .with_value(key2, "value2")
    .with_value(key3, "value3")
  
  assert_eq(ctx_with_values.values.length(), 3)
  match ctx_with_values.values[0] {
    (k, v) => {
      assert_eq(k, "first")
      assert_eq(v, "value1")
    }
    _ => @test.fail("Expected first key-value pair")
  }
  match ctx_with_values.values[1] {
    (k, v) => {
      assert_eq(k, "second")
      assert_eq(v, "value2")
    }
    _ => @test.fail("Expected second key-value pair")
  }
  match ctx_with_values.values[2] {
    (k, v) => {
      assert_eq(k, "third")
      assert_eq(v, "value3")
    }
    _ => @test.fail("Expected third key-value pair")
  }
}

test "context_key_creation" {
  let key1 = create_key("test_key")
  let key2 = create_key("another_key")
  
  assert_eq(key1.name, "test_key")
  assert_eq(key2.name, "another_key")
  assert_eq(key1.name != key2.name, true)
}

test "context_overwrite_value" {
  let ctx = Context::empty()
  let key = create_key("mutable_key")
  
  let ctx_with_first = ctx.with_value(key, "first_value")
  let ctx_with_second = ctx_with_first.with_value(key, "second_value")
  
  // 在当前实现中，with_value会添加新值而不是覆盖
  assert_eq(ctx_with_first.values.length(), 1)
  assert_eq(ctx_with_second.values.length(), 2)
  
  // get应该返回最后设置的值
  match ctx_with_second.get(key) {
    Some(value) => assert_eq(value, "second_value")
    None => @test.fail("Expected value")
  }
}

test "baggage_empty_creation" {
  let baggage = Baggage::empty()
  assert_eq(baggage.entries.length(), 0)
}

test "baggage_with_entry" {
  let baggage = Baggage::empty()
  let baggage_with_entry = baggage.with_entry("key1", "value1")
  
  assert_eq(baggage_with_entry.entries.length(), 1)
  match baggage_with_entry.entries[0] {
    (k, v) => {
      assert_eq(k, "key1")
      assert_eq(v, "value1")
    }
    _ => @test.fail("Expected key-value entry")
  }
}

test "baggage_multiple_entries" {
  let baggage = Baggage::empty()
  let baggage_with_entries = baggage
    .with_entry("user_id", "12345")
    .with_entry("request_id", "req-67890")
    .with_entry("trace_id", "trace-abcdef")
  
  assert_eq(baggage_with_entries.entries.length(), 3)
  
  match baggage_with_entries.get("user_id") {
    Some(value) => assert_eq(value, "12345")
    None => @test.fail("Expected user_id")
  }
  
  match baggage_with_entries.get("request_id") {
    Some(value) => assert_eq(value, "req-67890")
    None => @test.fail("Expected request_id")
  }
  
  match baggage_with_entries.get("trace_id") {
    Some(value) => assert_eq(value, "trace-abcdef")
    None => @test.fail("Expected trace_id")
  }
}

test "baggage_get_nonexistent_entry" {
  let baggage = Baggage::empty()
  let baggage_with_entry = baggage.with_entry("existing_key", "existing_value")
  
  match baggage_with_entry.get("nonexistent_key") {
    Some(_) => @test.fail("Expected None for nonexistent entry")
    None => @test.succeed()
  }
}

test "context_and_baggage_integration" {
  let ctx = Context::empty()
  let baggage = Baggage::empty()
  
  let user_key = create_key("user_context")
  let ctx_with_user = ctx.with_value(user_key, "user123")
  
  let baggage_with_data = baggage
    .with_entry("user_id", "123")
    .with_entry("session_id", "456")
  
  // 验证Context和Baggage可以独立使用
  match ctx_with_user.get(user_key) {
    Some(value) => assert_eq(value, "user123")
    None => @test.fail("Expected user context value")
  }
  
  assert_eq(baggage_with_data.entries.length(), 2)
  match baggage_with_data.get("user_id") {
    Some(value) => assert_eq(value, "123")
    None => @test.fail("Expected baggage user_id")
  }
}