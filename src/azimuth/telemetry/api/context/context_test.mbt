test "context_empty_creation" {
  // 测试空Context创建
  let context = Context::empty()
  assert_eq(context.values.size(), 0)
}

test "context_with_value_and_get" {
  // 测试Context的值设置和获取
  let context = Context::empty()
  let string_key = create_key("test_string")
  let int_key = create_key("test_int")
  
  let context_with_string = context.with_value(string_key, "hello")
  let context_with_both = context_with_string.with_value(int_key, "42")
  
  let retrieved_string = context_with_both.get(string_key)
  let retrieved_int = context_with_both.get(int_key)
  
  assert_eq(retrieved_string, Some("hello"))
  assert_eq(retrieved_int, Some("42"))
}

test "context_get_missing_key" {
  // 测试获取不存在的键
  let context = Context::empty()
  let missing_key = create_key("missing")
  
  let result = context.get(missing_key)
  assert_eq(result, None)
}

test "baggage_empty_creation" {
  // 测试空Baggage创建
  let baggage = Baggage::empty()
  assert_eq(baggage.entries.size(), 0)
}

test "baggage_with_entry_and_get" {
  // 测试Baggage的条目设置和获取
  let baggage = Baggage::empty()
  let baggage_with_entry = baggage.with_entry("user.id", "12345")
  
  let retrieved = baggage_with_entry.get("user.id")
  assert_eq(retrieved, Some("12345"))
  
  let missing = baggage_with_entry.get("missing.key")
  assert_eq(missing, None)
}

test "baggage_multiple_entries" {
  // 测试Baggage多个条目
  let baggage = Baggage::empty()
    .with_entry("user.id", "12345")
    .with_entry("service.version", "1.0.0")
    .with_entry("region", "us-west-2")
  
  assert_eq(baggage.get("user.id"), Some("12345"))
  assert_eq(baggage.get("service.version"), Some("1.0.0"))
  assert_eq(baggage.get("region"), Some("us-west-2"))
  assert_eq(baggage.get("missing"), None)
}

test "context_key_creation" {
  // 测试ContextKey创建
  let string_key = create_key("string_key")
  let int_key = create_key("int_key")
  
  assert_eq(string_key.name, "string_key")
  assert_eq(int_key.name, "int_key")
}

test "context_chained_operations" {
  // 测试Context的链式操作
  let user_key = create_key("user")
  let role_key = create_key("role")
  let session_key = create_key("session")
  
  let context = Context::empty()
    .with_value(user_key, "alice")
    .with_value(role_key, "admin")
    .with_value(session_key, "abc123")
  
  assert_eq(context.get(user_key), Some("alice"))
  assert_eq(context.get(role_key), Some("admin"))
  assert_eq(context.get(session_key), Some("abc123"))
  
  // 测试值覆盖
  let updated_context = context.with_value(role_key, "superadmin")
  assert_eq(updated_context.get(role_key), Some("superadmin"))
}

test "baggage_complex_operations" {
  // 测试Baggage的复杂操作
  let initial_baggage = Baggage::empty()
    .with_entry("user.id", "12345")
    .with_entry("service.name", "auth-service")
  
  // 链式添加更多条目
  let full_baggage = initial_baggage
    .with_entry("request.id", "req-67890")
    .with_entry("trace.sampled", "true")
    .with_entry("region", "us-west-2")
  
  // 验证所有条目
  assert_eq(full_baggage.get("user.id"), Some("12345"))
  assert_eq(full_baggage.get("service.name"), Some("auth-service"))
  assert_eq(full_baggage.get("request.id"), Some("req-67890"))
  assert_eq(full_baggage.get("trace.sampled"), Some("true"))
  assert_eq(full_baggage.get("region"), Some("us-west-2"))
  
  // 验证不存在的键
  assert_eq(full_baggage.get("nonexistent"), None)
  assert_eq(full_baggage.get("user.name"), None)
}