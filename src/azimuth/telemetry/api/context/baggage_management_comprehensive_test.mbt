// Baggage管理综合测试
test "baggage_basic_operations" {
  // 测试Baggage的基本操作
  
  let empty_baggage = Baggage::empty()
  assert_eq(empty_baggage.entries.length(), 0)
  
  // 添加条目
  let baggage1 = empty_baggage.with_entry("user-id", "12345")
  assert_eq(baggage1.entries.length(), 1)
  assert_eq(baggage1.entries[0].0, "user-id")
  assert_eq(baggage1.entries[0].1, "12345")
  
  // 获取条目
  match baggage1.get("user-id") {
    Some(value) => assert_eq(value, "12345")
    None => @test.fail("Test failed")
  }
  
  // 测试不存在的键
  match baggage1.get("non-existent") {
    Some(_) => @test.fail("Test failed")
    None => assert_eq(true, true)
  }
}

test "baggage_multiple_entries" {
  // 测试Baggage的多个条目管理
  
  let baggage = Baggage::empty()
    .with_entry("user-id", "12345")
    .with_entry("session-id", "abcdef-67890")
    .with_entry("request-id", "req-123456")
    .with_entry("trace-origin", "frontend")
    .with_entry("environment", "production")
  
  // 验证条目数量
  assert_eq(baggage.entries.length(), 5)
  
  // 验证所有条目都存在
  match baggage.get("user-id") {
    Some(value) => assert_eq(value, "12345")
    None => @test.fail("Test failed")
  }
  
  match baggage.get("session-id") {
    Some(value) => assert_eq(value, "abcdef-67890")
    None => @test.fail("Test failed")
  }
  
  match baggage.get("request-id") {
    Some(value) => assert_eq(value, "req-123456")
    None => @test.fail("Test failed")
  }
  
  match baggage.get("trace-origin") {
    Some(value) => assert_eq(value, "frontend")
    None => @test.fail("Test failed")
  }
  
  match baggage.get("environment") {
    Some(value) => assert_eq(value, "production")
    None => @test.fail("Test failed")
  }
}

test "baggage_key_value_variations" {
  // 测试Baggage的各种键值变体
  
  let baggage = Baggage::empty()
    .with_entry("empty-value", "")
    .with_entry("spaces", "value with spaces")
    .with_entry("special-chars", "!@#$%^&*()")
    .with_entry("unicode", "测试值🚀")
    .with_entry("numbers", "1234567890")
    .with_entry("mixed", "abc123!@#")
    .with_entry("long-key", "this-is-a-very-long-key-name-with-many-parts")
    .with_entry("long-value", "this-is-a-very-long-value-with-lots-of-content-and-details")
  
  // 验证所有变体
  match baggage.get("empty-value") {
    Some(value) => assert_eq(value, "")
    None => @test.fail("Test failed")
  }
  
  match baggage.get("spaces") {
    Some(value) => assert_eq(value, "value with spaces")
    None => @test.fail("Test failed")
  }
  
  match baggage.get("special-chars") {
    Some(value) => assert_eq(value, "!@#$%^&*()")
    None => @test.fail("Test failed")
  }
  
  match baggage.get("unicode") {
    Some(value) => assert_eq(value, "测试值🚀")
    None => @test.fail("Test failed")
  }
  
  match baggage.get("numbers") {
    Some(value) => assert_eq(value, "1234567890")
    None => @test.fail("Test failed")
  }
  
  match baggage.get("mixed") {
    Some(value) => assert_eq(value, "abc123!@#")
    None => @test.fail("Test failed")
  }
  
  match baggage.get("long-key") {
    Some(value) => assert_eq(value, "this-is-a-very-long-value-with-lots-of-content-and-details")
    None => @test.fail("Test failed")
  }
  
  match baggage.get("long-value") {
    Some(value) => assert_eq(value, "this-is-a-very-long-value-with-lots-of-content-and-details")
    None => @test.fail("Test failed")
  }
}

test "context_with_baggage_integration" {
  // 测试Context与Baggage的集成
  
  let user_key = create_key("user-id")
  let session_key = create_key("session-id")
  let trace_key = create_key("trace-id")
  
  // 创建带有值的Context
  let context = Context::empty()
    .with_value(user_key, "user-12345")
    .with_value(session_key, "session-abcdef")
    .with_value(trace_key, "trace-123456789")
  
  // 创建Baggage
  let baggage = Baggage::empty()
    .with_entry("baggage-user", "baggage-user-123")
    .with_entry("baggage-session", "baggage-session-456")
    .with_entry("baggage-env", "staging")
  
  // 验证Context值
  match context.get(user_key) {
    Some(value) => assert_eq(value, "user-12345")
    None => @test.fail("Test failed")
  }
  
  match context.get(session_key) {
    Some(value) => assert_eq(value, "session-abcdef")
    None => @test.fail("Test failed")
  }
  
  match context.get(trace_key) {
    Some(value) => assert_eq(value, "trace-123456789")
    None => @test.fail("Test failed")
  }
  
  // 验证Baggage条目
  assert_eq(baggage.entries.length(), 3)
  
  match baggage.get("baggage-user") {
    Some(value) => assert_eq(value, "baggage-user-123")
    None => @test.fail("Test failed")
  }
  
  match baggage.get("baggage-session") {
    Some(value) => assert_eq(value, "baggage-session-456")
    None => @test.fail("Test failed")
  }
  
  match baggage.get("baggage-env") {
    Some(value) => assert_eq(value, "staging")
    None => @test.fail("Test failed")
  }
}

test "baggage_case_sensitivity" {
  // 测试Baggage的键大小写敏感性
  
  let baggage = Baggage::empty()
    .with_entry("User-ID", "value1")
    .with_entry("user-id", "value2")
    .with_entry("USER-ID", "value3")
    .with_entry("user-Id", "value4")
  
  // 验证大小写敏感性（每个键都应该不同）
  assert_eq(baggage.entries.length(), 4)
  
  match baggage.get("User-ID") {
    Some(value) => assert_eq(value, "value1")
    None => @test.fail("Test failed")
  }
  
  match baggage.get("user-id") {
    Some(value) => assert_eq(value, "value2")
    None => @test.fail("Test failed")
  }
  
  match baggage.get("USER-ID") {
    Some(value) => assert_eq(value, "value3")
    None => @test.fail("Test failed")
  }
  
  match baggage.get("user-Id") {
    Some(value) => assert_eq(value, "value4")
    None => @test.fail("Test failed")
  }
}

test "baggage_edge_cases" {
  // 测试Baggage的边界情况
  
  // 测试空键
  let baggage1 = Baggage::empty().with_entry("", "empty-key-value")
  match baggage1.get("") {
    Some(value) => assert_eq(value, "empty-key-value")
    None => @test.fail("Test failed")
  }
  
  // 测试只包含空格的键
  let baggage2 = baggage1.with_entry("   ", "spaces-key")
  match baggage2.get("   ") {
    Some(value) => assert_eq(value, "spaces-key")
    None => @test.fail("Test failed")
  }
  
  // 测试只包含特殊字符的键
  let baggage3 = baggage2.with_entry("!@#$%", "special-chars-key")
  match baggage3.get("!@#$%") {
    Some(value) => assert_eq(value, "special-chars-key")
    None => @test.fail("Test failed")
  }
  
  // 测试非常长的键和值
  let long_key = "k".repeat(1000)
  let long_value = "v".repeat(1000)
  let baggage4 = baggage3.with_entry(long_key, long_value)
  
  match baggage4.get(long_key) {
    Some(value) => assert_eq(value, long_value)
    None => @test.fail("Test failed")
  }
  
  // 验证总条目数
  assert_eq(baggage4.entries.length(), 5)
}

test "context_key_creation_and_usage" {
  // 测试ContextKey的创建和使用
  
  // 创建各种类型的键
  let simple_key = create_key("simple")
  let complex_key = create_key("complex.key.with.dots")
  let spaced_key = create_key("key with spaces")
  let unicode_key = create_key("键测试🚀")
  let special_key = create_key("special!@#$%^&*()")
  let empty_key = create_key("")
  
  // 创建Context并添加值
  let context = Context::empty()
    .with_value(simple_key, "simple-value")
    .with_value(complex_key, "complex-value")
    .with_value(spaced_key, "spaced-value")
    .with_value(unicode_key, "unicode-value")
    .with_value(special_key, "special-value")
    .with_value(empty_key, "empty-key-value")
  
  // 验证所有值
  match context.get(simple_key) {
    Some(value) => assert_eq(value, "simple-value")
    None => @test.fail("Test failed")
  }
  
  match context.get(complex_key) {
    Some(value) => assert_eq(value, "complex-value")
    None => @test.fail("Test failed")
  }
  
  match context.get(spaced_key) {
    Some(value) => assert_eq(value, "spaced-value")
    None => @test.fail("Test failed")
  }
  
  match context.get(unicode_key) {
    Some(value) => assert_eq(value, "unicode-value")
    None => @test.fail("Test failed")
  }
  
  match context.get(special_key) {
    Some(value) => assert_eq(value, "special-value")
    None => @test.fail("Test failed")
  }
  
  match context.get(empty_key) {
    Some(value) => assert_eq(value, "empty-key-value")
    None => @test.fail("Test failed")
  }
}

test "baggage_large_scale_operations" {
  // 测试Baggage的大规模操作
  
  let mut baggage = Baggage::empty()
  
  // 添加大量条目
  let mut i = 0
  while i < 100 {
    let key = "key-" + i.to_string()
    let value = "value-" + i.to_string()
    baggage = baggage.with_entry(key, value)
    i = i + 1
  }
  
  // 验证条目数量
  assert_eq(baggage.entries.length(), 100)
  
  // 验证特定条目
  match baggage.get("key-0") {
    Some(value) => assert_eq(value, "value-0")
    None => @test.fail("Test failed")
  }
  
  match baggage.get("key-50") {
    Some(value) => assert_eq(value, "value-50")
    None => @test.fail("Test failed")
  }
  
  match baggage.get("key-99") {
    Some(value) => assert_eq(value, "value-99")
    None => @test.fail("Test failed")
  }
  
  // 验证不存在的条目
  match baggage.get("key-100") {
    Some(_) => @test.fail("Test failed")
    None => assert_eq(true, true)
  }
  
  match baggage.get("non-existent") {
    Some(_) => @test.fail("Test failed")
    None => assert_eq(true, true)
  }
}