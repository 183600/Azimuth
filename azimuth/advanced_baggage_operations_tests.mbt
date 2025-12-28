// 高级Baggage操作测试用例
// 测试复杂的Baggage传播、管理和边界条件

test "baggage_complex_propagation_chain" {
  // 测试复杂的baggage传播链
  let initial_baggage = Baggage::new()
  
  // 设置多个baggage条目
  let baggage1 = Baggage::set_entry(initial_baggage, "user.id", "user123")
  let baggage2 = Baggage::set_entry(baggage1, "request.id", "req456")
  let baggage3 = Baggage::set_entry(baggage2, "session.id", "session789")
  
  // 验证baggage条目
  assert_eq(Baggage::get_entry(baggage3, "user.id"), Some("user123"))
  assert_eq(Baggage::get_entry(baggage3, "request.id"), Some("req456"))
  assert_eq(Baggage::get_entry(baggage3, "session.id"), Some("session789"))
  
  // 测试baggage条目覆盖
  let baggage4 = Baggage::set_entry(baggage3, "user.id", "user456")
  assert_eq(Baggage::get_entry(baggage4, "user.id"), Some("user456"))
  
  // 测试baggage条目移除
  let baggage5 = Baggage::remove_entry(baggage4, "request.id")
  assert_eq(Baggage::get_entry(baggage5, "request.id"), None)
  assert_eq(Baggage::get_entry(baggage5, "user.id"), Some("user456"))
  assert_eq(Baggage::get_entry(baggage5, "session.id"), Some("session789"))
}

test "baggage_edge_cases_and_validation" {
  // 测试baggage的边界条件和验证
  let baggage = Baggage::new()
  
  // 测试空键和空值
  let empty_key_baggage = Baggage::set_entry(baggage, "", "empty_key_value")
  let empty_value_baggage = Baggage::set_entry(empty_key_baggage, "empty_value_key", "")
  
  // 测试特殊字符
  let special_chars_baggage = Baggage::set_entry(empty_value_baggage, "special.chars", "value!@#$%^&*()")
  let unicode_baggage = Baggage::set_entry(special_chars_baggage, "unicode.key", "测试值🚀")
  
  // 测试长键和长值
  let long_key = "this.is.a.very.long.key.name.that.exceeds.normal.length.expectations.for.testing.purposes"
  let long_value = "this.is.a.very.long.value.that.exceeds.normal.length.expectations.for.testing.purposes.and.should.still.work.correctly"
  let long_baggage = Baggage::set_entry(unicode_baggage, long_key, long_value)
  
  // 验证所有条目都能正确存储和检索
  assert_eq(Baggage::get_entry(long_baggage, ""), Some("empty_key_value"))
  assert_eq(Baggage::get_entry(long_baggage, "empty_value_key"), Some(""))
  assert_eq(Baggage::get_entry(long_baggage, "special.chars"), Some("value!@#$%^&*()"))
  assert_eq(Baggage::get_entry(long_baggage, "unicode.key"), Some("测试值🚀"))
  assert_eq(Baggage::get_entry(long_baggage, long_key), Some(long_value))
}

test "baggage_concurrent_operations" {
  // 测试baggage的并发操作
  let base_baggage = Baggage::new()
  
  // 模拟并发设置多个条目
  let baggage_a = Baggage::set_entry(base_baggage, "concurrent.a", "value_a")
  let baggage_b = Baggage::set_entry(base_baggage, "concurrent.b", "value_b")
  let baggage_c = Baggage::set_entry(base_baggage, "concurrent.c", "value_c")
  
  // 模拟并发读取操作
  let value_a = Baggage::get_entry(baggage_a, "concurrent.a")
  let value_b = Baggage::get_entry(baggage_b, "concurrent.b")
  let value_c = Baggage::get_entry(baggage_c, "concurrent.c")
  
  assert_eq(value_a, Some("value_a"))
  assert_eq(value_b, Some("value_b"))
  assert_eq(value_c, Some("value_c"))
  
  // 测试并发修改和删除
  let modified_a = Baggage::set_entry(baggage_a, "concurrent.a", "modified_value_a")
  let removed_b = Baggage::remove_entry(baggage_b, "concurrent.b")
  
  assert_eq(Baggage::get_entry(modified_a, "concurrent.a"), Some("modified_value_a"))
  assert_eq(Baggage::get_entry(removed_b, "concurrent.b"), None)
}

test "baggage_context_integration" {
  // 测试baggage与Context的集成
  let ctx = Context::root()
  let baggage_key = ContextKey::new("baggage")
  
  // 创建baggage并存储到context中
  let baggage = Baggage::new()
  let with_user = Baggage::set_entry(baggage, "user.id", "user123")
  let with_session = Baggage::set_entry(with_user, "session.id", "session456")
  
  // 将baggage信息存储到context（简化实现）
  let ctx_with_baggage = Context::with_value(ctx, baggage_key, "baggage_data")
  
  // 验证context中的baggage信息
  let retrieved_baggage = Context::get(ctx_with_baggage, baggage_key)
  assert_eq(retrieved_baggage, Some("baggage_data"))
  
  // 测试跨context传播
  let child_ctx = Context::with_value(ctx_with_baggage, ContextKey::new("child.data"), "child_value")
  let parent_baggage = Context::get(child_ctx, baggage_key)
  let child_data = Context::get(child_ctx, ContextKey::new("child.data"))
  
  assert_eq(parent_baggage, Some("baggage_data"))
  assert_eq(child_data, Some("child_value"))
}