// Context API 并发和边界测试用例

test "context_concurrent_operations" {
  // 测试上下文的并发操作安全性
  
  let base_context = Context::empty()
  let key1 = create_key("key1")
  let key2 = create_key("key2")
  let key3 = create_key("key3")
  
  // 模拟并发设置操作
  let context1 = base_context.with_value(key1, "value1")
  let context2 = context1.with_value(key2, "value2")
  let context3 = context2.with_value(key3, "value3")
  
  // 验证每个上下文都有正确的值
  assert_eq(context1.get(key1), Some("value1"))
  assert_eq(context1.get(key2), None)
  assert_eq(context1.get(key3), None)
  
  assert_eq(context2.get(key1), Some("value1"))
  assert_eq(context2.get(key2), Some("value2"))
  assert_eq(context2.get(key3), None)
  
  assert_eq(context3.get(key1), Some("value1"))
  assert_eq(context3.get(key2), Some("value2"))
  assert_eq(context3.get(key3), Some("value3"))
  
  // 验证原始上下文未被修改
  assert_eq(base_context.get(key1), None)
  assert_eq(base_context.get(key2), None)
  assert_eq(base_context.get(key3), None)
}

test "context_key_collision_handling" {
  // 测试上下文键冲突处理
  
  let base_context = Context::empty()
  let key = create_key("duplicate_key")
  
  // 设置相同的键多次
  let context1 = base_context.with_value(key, "first_value")
  let context2 = context1.with_value(key, "second_value")
  let context3 = context2.with_value(key, "third_value")
  
  // 验证最后设置的值生效
  assert_eq(context1.get(key), Some("first_value"))
  assert_eq(context2.get(key), Some("second_value"))
  assert_eq(context3.get(key), Some("third_value"))
  
  // 验证不同键不会冲突
  let different_key = create_key("different_key")
  let context4 = context3.with_value(different_key, "different_value")
  
  assert_eq(context4.get(key), Some("third_value"))
  assert_eq(context4.get(different_key), Some("different_value"))
}

test "context_large_value_handling" {
  // 测试上下文大值处理
  
  let base_context = Context::empty()
  let key = create_key("large_data")
  
  // 创建大字符串值
  let large_value = "x".to_string().repeat(10000)  // 10KB字符串
  let very_large_value = "y".to_string().repeat(100000)  // 100KB字符串
  
  // 测试大值存储
  let context1 = base_context.with_value(key, large_value)
  assert_eq(context1.get(key), Some(large_value))
  
  // 测试超大值存储
  let context2 = context1.with_value(key, very_large_value)
  assert_eq(context2.get(key), Some(very_large_value))
  
  // 测试多个大值
  let key2 = create_key("large_data2")
  let key3 = create_key("large_data3")
  let context3 = context2.with_value(key2, large_value)
  let context4 = context3.with_value(key3, very_large_value)
  
  assert_eq(context4.get(key2), Some(large_value))
  assert_eq(context4.get(key3), Some(very_large_value))
}

test "baggage_concurrent_entry_operations" {
  // 测试baggage的并发条目操作
  
  let base_baggage = Baggage::empty()
  
  // 模拟并发添加操作
  let baggage1 = base_baggage.with_entry("user.id", "12345")
  let baggage2 = baggage1.with_entry("request.id", "req-67890")
  let baggage3 = baggage2.with_entry("session.id", "session-abcdef")
  
  // 验证每个baggage都有正确的条目
  assert_eq(baggage1.get("user.id"), Some("12345"))
  assert_eq(baggage1.get("request.id"), None)
  assert_eq(baggage1.get("session.id"), None)
  
  assert_eq(baggage2.get("user.id"), Some("12345"))
  assert_eq(baggage2.get("request.id"), Some("req-67890"))
  assert_eq(baggage2.get("session.id"), None)
  
  assert_eq(baggage3.get("user.id"), Some("12345"))
  assert_eq(baggage3.get("request.id"), Some("req-67890"))
  assert_eq(baggage3.get("session.id"), Some("session-abcdef"))
  
  // 验证原始baggage未被修改
  assert_eq(base_baggage.get("user.id"), None)
  assert_eq(base_baggage.get("request.id"), None)
  assert_eq(base_baggage.get("session.id"), None)
}

test "baggage_special_characters_handling" {
  // 测试baggage特殊字符处理
  
  let base_baggage = Baggage::empty()
  
  // 测试包含特殊字符的键和值
  let special_entries = [
    ("key.with.dots", "value.with.dots"),
    ("key_with_underscores", "value_with_underscores"),
    ("key-with-hyphens", "value-with-hyphens"),
    ("key with spaces", "value with spaces"),
    ("key/with/slashes", "value/with/slashes"),
    ("key=with=equals", "value=with=equals"),
    ("key%20encoded", "value%20encoded"),
    ("key@with@symbols", "value@with@symbols")
  ]
  
  let mut current_baggage = base_baggage
  let mut i = 0
  while i < special_entries.length() {
    let (key, value) = special_entries[i]
    current_baggage = current_baggage.with_entry(key, value)
    i = i + 1
  }
  
  // 验证所有特殊字符条目都能正确存储和检索
  i = 0
  while i < special_entries.length() {
    let (key, value) = special_entries[i]
    assert_eq(current_baggage.get(key), Some(value))
    i = i + 1
  }
}

test "context_and_baggage_integration" {
  // 测试上下文和baggage的集成使用
  
  let base_context = Context::empty()
  let base_baggage = Baggage::empty()
  
  // 在上下文中存储复杂值
  let complex_key = create_key("complex_data")
  let complex_value = "user:123|role:admin|session:xyz|timestamp:1234567890"
  let context_with_complex = base_context.with_value(complex_key, complex_value)
  
  // 在baggage中存储相关元数据
  let baggage_with_meta = base_baggage
    .with_entry("user.id", "123")
    .with_entry("user.role", "admin")
    .with_entry("session.id", "xyz")
  
  // 验证集成操作
  assert_eq(context_with_complex.get(complex_key), Some(complex_value))
  assert_eq(baggage_with_meta.get("user.id"), Some("123"))
  assert_eq(baggage_with_meta.get("user.role"), Some("admin"))
  assert_eq(baggage_with_meta.get("session.id"), Some("xyz"))
  
  // 验证数据隔离（上下文和baggage互不影响）
  assert_eq(context_with_complex.get(create_key("user.id")), None)
  assert_eq(baggage_with_meta.get("complex_data"), None)
}