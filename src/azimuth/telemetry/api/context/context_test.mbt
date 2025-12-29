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

test "context_immutability_guarantees" {
  // 测试Context的不可变性保证（模拟并发安全）
  let base_context = Context::empty()
  let user_key = create_key("user")
  let role_key = create_key("role")
  
  // 创建多个"并发"修改的context
  let context1 = base_context.with_value(user_key, "alice")
  let context2 = base_context.with_value(role_key, "admin")
  let context3 = context1.with_value(role_key, "user")
  let context4 = context2.with_value(user_key, "bob")
  
  // 验证原始context未被修改
  assert_eq(base_context.values.length(), 0)
  assert_eq(base_context.get(user_key), None)
  assert_eq(base_context.get(role_key), None)
  
  // 验证每个context的独立性
  assert_eq(context1.get(user_key), Some("alice"))
  assert_eq(context1.get(role_key), None)
  
  assert_eq(context2.get(user_key), None)
  assert_eq(context2.get(role_key), Some("admin"))
  
  assert_eq(context3.get(user_key), Some("alice"))
  assert_eq(context3.get(role_key), Some("user"))
  
  assert_eq(context4.get(user_key), Some("bob"))
  assert_eq(context4.get(role_key), Some("admin"))
}

test "baggage_immutability_guarantees" {
  // 测试Baggage的不可变性保证（模拟并发安全）
  let base_baggage = Baggage::empty()
  
  // 创建多个"并发"修改的baggage
  let baggage1 = base_baggage.with_entry("user.id", "12345")
  let baggage2 = base_baggage.with_entry("service.name", "auth")
  let baggage3 = baggage1.with_entry("request.id", "req-001")
  let baggage4 = baggage2.with_entry("trace.id", "trace-123")
  
  // 验证原始baggage未被修改
  assert_eq(base_baggage.entries.length(), 0)
  assert_eq(base_baggage.get("user.id"), None)
  assert_eq(base_baggage.get("service.name"), None)
  
  // 验证每个baggage的独立性
  assert_eq(baggage1.get("user.id"), Some("12345"))
  assert_eq(baggage1.get("service.name"), None)
  assert_eq(baggage1.get("request.id"), None)
  
  assert_eq(baggage2.get("user.id"), None)
  assert_eq(baggage2.get("service.name"), Some("auth"))
  assert_eq(baggage2.get("trace.id"), None)
  
  assert_eq(baggage3.get("user.id"), Some("12345"))
  assert_eq(baggage3.get("request.id"), Some("req-001"))
  assert_eq(baggage3.get("service.name"), None)
  
  assert_eq(baggage4.get("service.name"), Some("auth"))
  assert_eq(baggage4.get("trace.id"), Some("trace-123"))
  assert_eq(baggage4.get("user.id"), None)
}

test "context_concurrent_access_simulation" {
  // 模拟并发访问Context的场景
  let shared_context = Context::empty()
    .with_value(create_key("service"), "api")
    .with_value(create_key("version"), "1.0.0")
  
  // 模拟多个"线程"同时读取context
  let read_result1 = shared_context.get(create_key("service"))
  let read_result2 = shared_context.get(create_key("version"))
  let read_result3 = shared_context.get(create_key("nonexistent"))
  
  // 验证读取结果的一致性
  assert_eq(read_result1, Some("api"))
  assert_eq(read_result2, Some("1.0.0"))
  assert_eq(read_result3, None)
  
  // 模拟在"读取"的同时创建新的context
  let new_context1 = shared_context.with_value(create_key("request.id"), "req-001")
  let new_context2 = shared_context.with_value(create_key("user.id"), "user-123")
  
  // 验证原始context仍然保持不变
  assert_eq(shared_context.get(create_key("service")), Some("api"))
  assert_eq(shared_context.get(create_key("version")), Some("1.0.0"))
  assert_eq(shared_context.get(create_key("request.id")), None)
  assert_eq(shared_context.get(create_key("user.id")), None)
  
  // 验证新context的独立性
  assert_eq(new_context1.get(create_key("request.id")), Some("req-001"))
  assert_eq(new_context1.get(create_key("user.id")), None)
  
  assert_eq(new_context2.get(create_key("request.id")), None)
  assert_eq(new_context2.get(create_key("user.id")), Some("user-123"))
}

test "baggage_concurrent_access_simulation" {
  // 模拟并发访问Baggage的场景
  let shared_baggage = Baggage::empty()
    .with_entry("environment", "production")
    .with_entry("region", "us-west-2")
  
  // 模拟多个"线程"同时读取baggage
  let read_result1 = shared_baggage.get("environment")
  let read_result2 = shared_baggage.get("region")
  let read_result3 = shared_baggage.get("nonexistent")
  
  // 验证读取结果的一致性
  assert_eq(read_result1, Some("production"))
  assert_eq(read_result2, Some("us-west-2"))
  assert_eq(read_result3, None)
  
  // 模拟在"读取"的同时创建新的baggage
  let new_baggage1 = shared_baggage.with_entry("request.id", "req-001")
  let new_baggage2 = shared_baggage.with_entry("trace.id", "trace-123")
  
  // 验证原始baggage仍然保持不变
  assert_eq(shared_baggage.get("environment"), Some("production"))
  assert_eq(shared_baggage.get("region"), Some("us-west-2"))
  assert_eq(shared_baggage.get("request.id"), None)
  assert_eq(shared_baggage.get("trace.id"), None)
  
  // 验证新baggage的独立性
  assert_eq(new_baggage1.get("request.id"), Some("req-001"))
  assert_eq(new_baggage1.get("trace.id"), None)
  
  assert_eq(new_baggage2.get("request.id"), None)
  assert_eq(new_baggage2.get("trace.id"), Some("trace-123"))
}

test "context_key_collision_handling" {
  // 测试Context键冲突处理（模拟并发场景）
  let key = create_key("shared.key")
  let base_context = Context::empty()
  
  // 模拟多个"线程"使用相同的键设置不同的值
  let context1 = base_context.with_value(key, "value1")
  let context2 = base_context.with_value(key, "value2")
  let context3 = context1.with_value(key, "value3")
  let context4 = context2.with_value(key, "value4")
  
  // 验证每个context都有正确的值
  assert_eq(context1.get(key), Some("value1"))
  assert_eq(context2.get(key), Some("value2"))
  assert_eq(context3.get(key), Some("value3"))
  assert_eq(context4.get(key), Some("value4"))
  
  // 验证原始context未被修改
  assert_eq(base_context.get(key), None)
}

test "baggage_entry_collision_handling" {
  // 测试Baggage条目冲突处理（模拟并发场景）
  let base_baggage = Baggage::empty()
  
  // 模拟多个"线程"使用相同的键设置不同的值
  let baggage1 = base_baggage.with_entry("shared.key", "value1")
  let baggage2 = base_baggage.with_entry("shared.key", "value2")
  let baggage3 = baggage1.with_entry("shared.key", "value3")
  let baggage4 = baggage2.with_entry("shared.key", "value4")
  
  // 验证每个baggage都有正确的值
  assert_eq(baggage1.get("shared.key"), Some("value1"))
  assert_eq(baggage2.get("shared.key"), Some("value2"))
  assert_eq(baggage3.get("shared.key"), Some("value3"))
  assert_eq(baggage4.get("shared.key"), Some("value4"))
  
  // 验证原始baggage未被修改
  assert_eq(base_baggage.get("shared.key"), None)
}