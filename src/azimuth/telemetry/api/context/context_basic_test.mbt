// Context模块基本测试用例
test "context_basic_operations" {
  let ctx = Context::empty()
  let key = create_key("test_key")
  let ctx_with_value = ctx.with_value(key, "test_value")
  
  let retrieved_value = ctx_with_value.get(key)
  match retrieved_value {
    Some(value) => assert_eq(value, "test_value")
    None => @test.fail("Test failed")
  }
  
  // 测试不存在的键
  let non_existent_key = create_key("non_existent")
  let non_existent_value = ctx_with_value.get(non_existent_key)
  match non_existent_value {
    Some(_) => @test.fail("Test failed")
    None => assert_eq(true, true)
  }
}

test "baggage_basic_operations" {
  let baggage = Baggage::empty()
  let baggage_with_entry = baggage.with_entry("test_key", "test_value")
  
  let retrieved_value = baggage_with_entry.get("test_key")
  match retrieved_value {
    Some(value) => assert_eq(value, "test_value")
    None => @test.fail("Test failed")
  }
  
  // 测试不存在的键
  let non_existent_value = baggage_with_entry.get("non_existent")
  match non_existent_value {
    Some(_) => @test.fail("Test failed")
    None => assert_eq(true, true)
  }
}

test "context_key_creation" {
  let key1 = create_key("key1")
  let key2 = create_key("key2")
  
  assert_eq(key1.name, "key1")
  assert_eq(key2.name, "key2")
  if key1.name == key2.name {
    @test.fail("Test failed")
  }
}

test "context_multiple_values_and_overwrites" {
  let ctx = Context::empty()
  let key1 = create_key("user.id")
  let key2 = create_key("request.id")
  let key3 = create_key("session.token")
  
  // 添加多个值
  let ctx1 = ctx.with_value(key1, "user-123")
  let ctx2 = ctx1.with_value(key2, "req-456")
  let ctx3 = ctx2.with_value(key3, "token-abc")
  
  // 验证所有值都存在
  match ctx3.get(key1) {
    Some(value) => assert_eq(value, "user-123")
    None => @test.fail("Test failed")
  }
  
  match ctx3.get(key2) {
    Some(value) => assert_eq(value, "req-456")
    None => @test.fail("Test failed")
  }
  
  match ctx3.get(key3) {
    Some(value) => assert_eq(value, "token-abc")
    None => @test.fail("Test failed")
  }
  
  // 测试覆盖现有值
  let ctx4 = ctx3.with_value(key1, "user-789")
  match ctx4.get(key1) {
    Some(value) => assert_eq(value, "user-789")
    None => @test.fail("Test failed")
  }
  
  // 验证其他值不受影响
  match ctx4.get(key2) {
    Some(value) => assert_eq(value, "req-456")
    None => @test.fail("Test failed")
  }
}

test "context_empty_and_special_values" {
  let ctx = Context::empty()
  let empty_key = create_key("empty.value")
  let special_key = create_key("special.value")
  
  // 测试空字符串值
  let ctx_with_empty = ctx.with_value(empty_key, "")
  match ctx_with_empty.get(empty_key) {
    Some(value) => assert_eq(value, "")
    None => @test.fail("Test failed")
  }
  
  // 测试特殊字符值
  let special_value = "special!@#$%^&*()_+-={}[]|\\:;\"'<>?,./"
  let ctx_with_special = ctx_with_empty.with_value(special_key, special_value)
  match ctx_with_special.get(special_key) {
    Some(value) => assert_eq(value, special_value)
    None => @test.fail("Test failed")
  }
  
  // 测试长字符串值
  let long_value = "This is a very long string that contains many words and characters to test if the context can handle long values properly without any issues or truncation or other problems that might occur with string handling in the implementation."
  let long_key = create_key("long.value")
  let ctx_with_long = ctx_with_special.with_value(long_key, long_value)
  match ctx_with_long.get(long_key) {
    Some(value) => assert_eq(value, long_value)
    None => @test.fail("Test failed")
  }
}

test "baggage_multiple_entries_and_overwrites" {
  let baggage = Baggage::empty()
  
  // 添加多个条目
  let baggage1 = baggage.with_entry("user.id", "12345")
  let baggage2 = baggage1.with_entry("request.id", "67890")
  let baggage3 = baggage2.with_entry("session.id", "abcdef")
  let baggage4 = baggage3.with_entry("trace.id", "1234567890")
  
  // 验证所有条目都存在
  match baggage4.get("user.id") {
    Some(value) => assert_eq(value, "12345")
    None => @test.fail("Test failed")
  }
  
  match baggage4.get("request.id") {
    Some(value) => assert_eq(value, "67890")
    None => @test.fail("Test failed")
  }
  
  match baggage4.get("session.id") {
    Some(value) => assert_eq(value, "abcdef")
    None => @test.fail("Test failed")
  }
  
  match baggage4.get("trace.id") {
    Some(value) => assert_eq(value, "1234567890")
    None => @test.fail("Test failed")
  }
  
  // 测试覆盖现有条目
  let baggage5 = baggage4.with_entry("user.id", "54321")
  match baggage5.get("user.id") {
    Some(value) => assert_eq(value, "54321")
    None => @test.fail("Test failed")
  }
  
  // 验证其他条目不受影响
  match baggage5.get("request.id") {
    Some(value) => assert_eq(value, "67890")
    None => @test.fail("Test failed")
  }
}

test "context_and_baggage_integration" {
  let ctx = Context::empty()
  let baggage = Baggage::empty()
  
  // 在context中添加值
  let trace_key = create_key("trace.id")
  let span_key = create_key("span.id")
  let ctx_with_values = ctx
    .with_value(trace_key, "trace-123")
    .with_value(span_key, "span-456")
  
  // 在baggage中添加相关值
  let baggage_with_entries = baggage
    .with_entry("user.id", "user-789")
    .with_entry("correlation.id", "corr-012")
  
  // 验证context和baggage中的值
  match ctx_with_values.get(trace_key) {
    Some(value) => assert_eq(value, "trace-123")
    None => @test.fail("Test failed")
  }
  
  match ctx_with_values.get(span_key) {
    Some(value) => assert_eq(value, "span-456")
    None => @test.fail("Test failed")
  }
  
  match baggage_with_entries.get("user.id") {
    Some(value) => assert_eq(value, "user-789")
    None => @test.fail("Test failed")
  }
  
  match baggage_with_entries.get("correlation.id") {
    Some(value) => assert_eq(value, "corr-012")
    None => @test.fail("Test failed")
  }
  
  // 测试空值和特殊键名
  let special_ctx = ctx_with_values
    .with_value(create_key(""), "empty.key.value")
    .with_value(create_key("special.key.with.dots"), "special.value")
    .with_value(create_key("key with spaces"), "spaces.value")
  
  match special_ctx.get(create_key("")) {
    Some(value) => assert_eq(value, "empty.key.value")
    None => @test.fail("Test failed")
  }
  
  let special_baggage = baggage_with_entries
    .with_entry("", "empty.entry.value")
    .with_entry("special.entry.with.dots", "special.entry.value")
    .with_entry("entry with spaces", "spaces.entry.value")
  
  match special_baggage.get("") {
    Some(value) => assert_eq(value, "empty.entry.value")
    None => @test.fail("Test failed")
  }
}