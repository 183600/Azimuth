// 上下文API操作测试用例
// 测试Context和Baggage的各种操作

use azimuth.telemetry.api.context.{Context, ContextKey, Baggage, create_key}

test "context_empty_creation" {
  // 测试空上下文创建
  
  let empty_ctx = Context::empty()
  
  // 验证空上下文
  assert_eq(empty_ctx.values.length(), 0)
  
  // 测试在空上下文中获取值
  let key = create_key("test_key")
  let result = empty_ctx.get(key)
  assert_eq(result, None)
}

test "context_with_value_operations" {
  // 测试上下文值的设置和获取
  
  let empty_ctx = Context::empty()
  let key1 = create_key("user_id")
  let key2 = create_key("session_id")
  let key3 = create_key("request_id")
  
  // 设置第一个值
  let ctx1 = empty_ctx.with_value(key1, "user123")
  assert_eq(ctx1.values.length(), 1)
  
  // 获取第一个值
  match ctx1.get(key1) {
    Some(value) => assert_eq(value, "user123")
    None => assert_eq(false, true)
  }
  
  // 设置第二个值
  let ctx2 = ctx1.with_value(key2, "session456")
  assert_eq(ctx2.values.length(), 2)
  
  // 验证两个值都存在
  match ctx2.get(key1) {
    Some(value) => assert_eq(value, "user123")
    None => assert_eq(false, true)
  }
  
  match ctx2.get(key2) {
    Some(value) => assert_eq(value, "session456")
    None => assert_eq(false, true)
  }
  
  // 设置第三个值
  let ctx3 = ctx2.with_value(key3, "request789")
  assert_eq(ctx3.values.length(), 3)
  
  // 验证所有值都存在
  match ctx3.get(key1) {
    Some(value) => assert_eq(value, "user123")
    None => assert_eq(false, true)
  }
  
  match ctx3.get(key2) {
    Some(value) => assert_eq(value, "session456")
    None => assert_eq(false, true)
  }
  
  match ctx3.get(key3) {
    Some(value) => assert_eq(value, "request789")
    None => assert_eq(false, true)
  }
}

test "context_key_creation_and_properties" {
  // 测试上下文键的创建和属性
  
  let key1 = create_key("test_key")
  let key2 = create_key("another_key")
  let key3 = create_key("user.context")
  
  // 验证键名称
  assert_eq(key1.name, "test_key")
  assert_eq(key1.name.length(), 8)
  assert_eq(key1.name.has_prefix("test"), true)
  
  assert_eq(key2.name, "another_key")
  assert_eq(key2.name.length(), 11)
  assert_eq(key2.name.has_suffix("key"), true)
  
  assert_eq(key3.name, "user.context")
  assert_eq(key3.name.length(), 12)
  assert_eq(key3.name.contains("."), true)
}

test "context_nonexistent_key_handling" {
  // 测试处理不存在的键
  
  let ctx = Context::empty()
    .with_value(create_key("existing_key"), "existing_value")
  
  // 验证存在的键
  match ctx.get(create_key("existing_key")) {
    Some(value) => assert_eq(value, "existing_value")
    None => assert_eq(false, true)
  }
  
  // 验证不存在的键
  match ctx.get(create_key("nonexistent_key")) {
    Some(_) => assert_eq(false, true)  // 不应该找到值
    None => assert_eq(true, true)      // 应该返回None
  }
}

test "baggage_empty_creation" {
  // 测试空行李创建
  
  let empty_baggage = Baggage::empty()
  
  // 验证空行李
  assert_eq(empty_baggage.entries.length(), 0)
  
  // 测试在空行李中获取值
  let result = empty_baggage.get("test_key")
  assert_eq(result, None)
}

test "baggage_entry_operations" {
  // 测试行李条目的设置和获取
  
  let empty_baggage = Baggage::empty()
  
  // 设置第一个条目
  let baggage1 = empty_baggage.with_entry("user_id", "user123")
  assert_eq(baggage1.entries.length(), 1)
  
  // 获取第一个条目
  match baggage1.get("user_id") {
    Some(value) => assert_eq(value, "user123")
    None => assert_eq(false, true)
  }
  
  // 设置更多条目
  let baggage2 = baggage1.with_entry("session_id", "session456")
  let baggage3 = baggage2.with_entry("request_id", "request789")
  
  // 验证条目数量
  assert_eq(baggage3.entries.length(), 3)
  
  // 验证所有条目都存在
  match baggage3.get("user_id") {
    Some(value) => assert_eq(value, "user123")
    None => assert_eq(false, true)
  }
  
  match baggage3.get("session_id") {
    Some(value) => assert_eq(value, "session456")
    None => assert_eq(false, true)
  }
  
  match baggage3.get("request_id") {
    Some(value) => assert_eq(value, "request789")
    None => assert_eq(false, true)
  }
}

test "baggage_nonexistent_entry_handling" {
  // 测试处理不存在的行李条目
  
  let baggage = Baggage::empty()
    .with_entry("existing_entry", "existing_value")
  
  // 验证存在的条目
  match baggage.get("existing_entry") {
    Some(value) => assert_eq(value, "existing_value")
    None => assert_eq(false, true)
  }
  
  // 验证不存在的条目
  match baggage.get("nonexistent_entry") {
    Some(_) => assert_eq(false, true)  // 不应该找到值
    None => assert_eq(true, true)      // 应该返回None
  }
}

test "context_and_baggage_integration" {
  // 测试上下文和行李的集成使用
  
  // 创建带有多个值的上下文
  let ctx = Context::empty()
    .with_value(create_key("trace_id"), "trace123")
    .with_value(create_key("span_id"), "span456")
  
  // 创建带有多个条目的行李
  let baggage = Baggage::empty()
    .with_entry("user_id", "user789")
    .with_entry("session_id", "session012")
  
  // 验证上下文值
  match ctx.get(create_key("trace_id")) {
    Some(value) => assert_eq(value, "trace123")
    None => assert_eq(false, true)
  }
  
  match ctx.get(create_key("span_id")) {
    Some(value) => assert_eq(value, "span456")
    None => assert_eq(false, true)
  }
  
  // 验证行李条目
  match baggage.get("user_id") {
    Some(value) => assert_eq(value, "user789")
    None => assert_eq(false, true)
  }
  
  match baggage.get("session_id") {
    Some(value) => assert_eq(value, "session012")
    None => assert_eq(false, true)
  }
  
  // 验证数量
  assert_eq(ctx.values.length(), 2)
  assert_eq(baggage.entries.length(), 2)
}

test "context_value_overwrite_behavior" {
  // 测试上下文值的覆盖行为
  
  let key = create_key("test_key")
  
  // 设置初始值
  let ctx1 = Context::empty().with_value(key, "initial_value")
  match ctx1.get(key) {
    Some(value) => assert_eq(value, "initial_value")
    None => assert_eq(false, true)
  }
  
  // 覆盖值
  let ctx2 = ctx1.with_value(key, "updated_value")
  match ctx2.get(key) {
    Some(value) => assert_eq(value, "updated_value")
    None => assert_eq(false, true)
  }
  
  // 验证上下文值的数量增加（因为实现是追加而非替换）
  assert_eq(ctx2.values.length(), 2)
}