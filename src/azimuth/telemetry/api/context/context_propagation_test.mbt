// API Context模块的上下文传播测试

test "context_creation_and_empty" {
  // 测试上下文创建和空上下文
  
  let empty_ctx = context::Context::empty()
  
  // 验证空上下文
  assert_eq(empty_ctx.values.length(), 0)
  
  // 验证从空上下文获取值返回None
  let key = context::create_key("test.key")
  let value = empty_ctx.get(key)
  assert_eq(value, None)
}

test "context_with_value_operations" {
  // 测试上下文的值操作
  
  let ctx = context::Context::empty()
  let key1 = context::create_key("user.id")
  let key2 = context::create_key("request.id")
  let key3 = context::create_key("trace.id")
  
  // 添加第一个值
  let ctx1 = ctx.with_value(key1, "user123")
  
  // 验证值被添加
  let value1 = ctx1.get(key1)
  match value1 {
    Some(v) => assert_eq(v, "user123")
    None => assert_eq(false, true)  // 不应该到达这里
  }
  
  // 验证原始上下文没有被修改
  let original_value = ctx.get(key1)
  assert_eq(original_value, None)
  
  // 添加第二个值
  let ctx2 = ctx1.with_value(key2, "req456")
  
  // 验证两个值都存在
  let value2 = ctx2.get(key2)
  match value2 {
    Some(v) => assert_eq(v, "req456")
    None => assert_eq(false, true)  // 不应该到达这里
  }
  
  // 验证第一个值仍然存在
  let value1_again = ctx2.get(key1)
  match value1_again {
    Some(v) => assert_eq(v, "user123")
    None => assert_eq(false, true)  // 不应该到达这里
  }
  
  // 添加第三个值
  let ctx3 = ctx2.with_value(key3, "trace789")
  
  // 验证所有三个值都存在
  let value3 = ctx3.get(key3)
  match value3 {
    Some(v) => assert_eq(v, "trace789")
    None => assert_eq(false, true)  // 不应该到达这里
  }
  
  // 验证值数组长度
  assert_eq(ctx3.values.length(), 3)
}

test "context_key_creation_and_usage" {
  // 测试上下文键的创建和使用
  
  let key_name = "correlation.id"
  let key = context::create_key(key_name)
  
  // 验证键名
  assert_eq(key.name, key_name)
  
  // 测试使用键存储和检索值
  let ctx = context::Context::empty()
  let ctx_with_value = ctx.with_value(key, "corr-12345")
  
  let retrieved_value = ctx_with_value.get(key)
  match retrieved_value {
    Some(v) => assert_eq(v, "corr-12345")
    None => assert_eq(false, true)  // 不应该到达这里
  }
  
  // 测试特殊字符的键名
  let special_key_name = "special.key.with.dots_and_underscores"
  let special_key = context::create_key(special_key_name)
  assert_eq(special_key.name, special_key_name)
  
  let ctx_with_special = ctx_with_value.with_value(special_key, "special-value")
  let special_value = ctx_with_special.get(special_key)
  match special_value {
    Some(v) => assert_eq(v, "special-value")
    None => assert_eq(false, true)  // 不应该到达这里
  }
}

test "context_value_overwrite" {
  // 测试上下文值的覆盖
  
  let ctx = context::Context::empty()
  let key = context::create_key("mutable.value")
  
  // 设置初始值
  let ctx1 = ctx.with_value(key, "initial")
  let value1 = ctx1.get(key)
  match value1 {
    Some(v) => assert_eq(v, "initial")
    None => assert_eq(false, true)  // 不应该到达这里
  }
  
  // 覆盖值
  let ctx2 = ctx1.with_value(key, "updated")
  let value2 = ctx2.get(key)
  match value2 {
    Some(v) => assert_eq(v, "updated")
    None => assert_eq(false, true)  // 不应该到达这里
  }
  
  // 验证值数组包含两个条目
  assert_eq(ctx2.values.length(), 2)
  
  // 再次覆盖
  let ctx3 = ctx2.with_value(key, "final")
  let value3 = ctx3.get(key)
  match value3 {
    Some(v) => assert_eq(v, "final")
    None => assert_eq(false, true)  // 不应该到达这里
  }
  
  // 验证值数组包含三个条目
  assert_eq(ctx3.values.length(), 3)
}

test "baggage_creation_and_empty" {
  // 测试Baggage创建和空Baggage
  
  let empty_baggage = context::Baggage::empty()
  
  // 验证空Baggage
  assert_eq(empty_baggage.entries.length(), 0)
  
  // 验证从空Baggage获取值返回None
  let value = empty_baggage.get("test.key")
  assert_eq(value, None)
}

test "baggage_entry_operations" {
  // 测试Baggage的条目操作
  
  let baggage = context::Baggage::empty()
  
  // 添加第一个条目
  let baggage1 = baggage.with_entry("user.id", "user123")
  
  // 验证条目被添加
  let value1 = baggage1.get("user.id")
  match value1 {
    Some(v) => assert_eq(v, "user123")
    None => assert_eq(false, true)  // 不应该到达这里
  }
  
  // 验证原始Baggage没有被修改
  let original_value = baggage.get("user.id")
  assert_eq(original_value, None)
  
  // 添加更多条目
  let baggage2 = baggage1.with_entry("request.id", "req456")
  let baggage3 = baggage2.with_entry("service.name", "auth-service")
  
  // 验证所有条目都存在
  let value2 = baggage3.get("request.id")
  match value2 {
    Some(v) => assert_eq(v, "req456")
    None => assert_eq(false, true)  // 不应该到达这里
  }
  
  let value3 = baggage3.get("service.name")
  match value3 {
    Some(v) => assert_eq(v, "auth-service")
    None => assert_eq(false, true)  // 不应该到达这里
  }
  
  // 验证条目数组长度
  assert_eq(baggage3.entries.length(), 3)
}

test "baggage_key_value_types" {
  // 测试Baggage的键值类型
  
  let baggage = context::Baggage::empty()
  
  // 测试各种键值类型
  let baggage1 = baggage.with_entry("numeric.key", "12345")
  let baggage2 = baggage1.with_entry("boolean.key", "true")
  let baggage3 = baggage2.with_entry("json.key", "{\"nested\": \"value\"}")
  let baggage4 = baggage3.with_entry("url.key", "https://example.com/path")
  let baggage5 = baggage4.with_entry("empty.key", "")
  
  // 验证数值键
  let numeric_value = baggage5.get("numeric.key")
  match numeric_value {
    Some(v) => assert_eq(v, "12345")
    None => assert_eq(false, true)  // 不应该到达这里
  }
  
  // 验证布尔键
  let boolean_value = baggage5.get("boolean.key")
  match boolean_value {
    Some(v) => assert_eq(v, "true")
    None => assert_eq(false, true)  // 不应该到达这里
  }
  
  // 验证JSON键
  let json_value = baggage5.get("json.key")
  match json_value {
    Some(v) => assert_eq(v, "{\"nested\": \"value\"}")
    None => assert_eq(false, true)  // 不应该到达这里
  }
  
  // 验证URL键
  let url_value = baggage5.get("url.key")
  match url_value {
    Some(v) => assert_eq(v, "https://example.com/path")
    None => assert_eq(false, true)  // 不应该到达这里
  }
  
  // 验证空值
  let empty_value = baggage5.get("empty.key")
  match empty_value {
    Some(v) => assert_eq(v, "")
    None => assert_eq(false, true)  // 不应该到达这里
  }
}

test "baggage_entry_overwrite" {
  // 测试Baggage条目的覆盖
  
  let baggage = context::Baggage::empty()
  
  // 设置初始条目
  let baggage1 = baggage.with_entry("overwrite.me", "initial")
  let value1 = baggage1.get("overwrite.me")
  match value1 {
    Some(v) => assert_eq(v, "initial")
    None => assert_eq(false, true)  // 不应该到达这里
  }
  
  // 覆盖条目
  let baggage2 = baggage1.with_entry("overwrite.me", "updated")
  let value2 = baggage2.get("overwrite.me")
  match value2 {
    Some(v) => assert_eq(v, "updated")
    None => assert_eq(false, true)  // 不应该到达这里
  }
  
  // 验证条目数组包含两个条目
  assert_eq(baggage2.entries.length(), 2)
  
  // 再次覆盖
  let baggage3 = baggage2.with_entry("overwrite.me", "final")
  let value3 = baggage3.get("overwrite.me")
  match value3 {
    Some(v) => assert_eq(v, "final")
    None => assert_eq(false, true)  // 不应该到达这里
  }
  
  // 验证条目数组包含三个条目
  assert_eq(baggage3.entries.length(), 3)
}

test "context_baggage_integration" {
  // 测试Context和Baggage的集成
  
  let ctx = context::Context::empty()
  let baggage = context::Baggage::empty()
  
  // 在Context中存储Baggage
  let baggage_key = context::create_key("otel.baggage")
  
  // 添加一些条目到Baggage
  let populated_baggage = baggage
    .with_entry("user.id", "user123")
    .with_entry("service.name", "payment-service")
    .with_entry("request.id", "req789")
  
  // 将Baggage存储在Context中（简化实现，实际中可能需要序列化）
  let ctx_with_baggage = ctx.with_value(baggage_key, "baggage_data")
  
  // 验证Baggage数据可以从Context中获取
  let stored_baggage_data = ctx_with_baggage.get(baggage_key)
  match stored_baggage_data {
    Some(data) => assert_eq(data, "baggage_data")
    None => assert_eq(false, true)  // 不应该到达这里
  }
  
  // 验证原始Baggage仍然独立
  let user_id = populated_baggage.get("user.id")
  match user_id {
    Some(id) => assert_eq(id, "user123")
    None => assert_eq(false, true)  // 不应该到达这里
  }
  
  // 验证Context中可以存储其他数据
  let trace_key = context::create_key("trace.id")
  let final_ctx = ctx_with_baggage.with_value(trace_key, "trace123")
  
  let trace_id = final_ctx.get(trace_key)
  match trace_id {
    Some(id) => assert_eq(id, "trace123")
    None => assert_eq(false, true)  // 不应该到达这里
  }
  
  // 验证Baggage数据仍然存在
  let baggage_data_still = final_ctx.get(baggage_key)
  match baggage_data_still {
    Some(data) => assert_eq(data, "baggage_data")
    None => assert_eq(false, true)  // 不应该到达这里
  }
}