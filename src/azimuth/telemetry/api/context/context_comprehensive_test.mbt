// Context模块的Baggage操作测试用例
test "baggage_basic_operations" {
  // 测试Baggage的基本操作
  let empty_baggage = Baggage::empty()
  assert_eq(empty_baggage.entries.length(), 0)
  
  // 添加单个条目
  let baggage1 = empty_baggage.with_entry("user.id", "12345")
  assert_eq(baggage1.entries.length(), 1)
  assert_eq(baggage1.entries[0].0, "user.id")
  assert_eq(baggage1.entries[0].1, "12345")
  
  // 添加多个条目
  let baggage2 = baggage1.with_entry("request.id", "req-67890")
  assert_eq(baggage2.entries.length(), 2)
  
  // 获取条目
  match baggage2.get("user.id") {
    Some(value) => assert_eq(value, "12345")
    None => @test.fail("Test failed")
  }
  
  match baggage2.get("request.id") {
    Some(value) => assert_eq(value, "req-67890")
    None => @test.fail("Test failed")
  }
  
  // 获取不存在的条目
  match baggage2.get("nonexistent.key") {
    Some(_) => @test.fail("Test failed")
    None => assert_eq(true, true)
  }
}

test "baggage_complex_scenarios" {
  // 测试Baggage的复杂场景
  
  // 创建包含多个条目的baggage
  let baggage = Baggage::empty()
    .with_entry("service.name", "payment-service")
    .with_entry("service.version", "1.2.3")
    .with_entry("deployment.environment", "production")
    .with_entry("trace.sampled", "true")
    .with_entry("user.id", "user-123")
    .with_entry("request.id", "req-456")
    .with_entry("session.id", "session-789")
    .with_entry("correlation.id", "corr-012")
  
  assert_eq(baggage.entries.length(), 8)
  
  // 验证所有条目
  let expected_entries = [
    ("service.name", "payment-service"),
    ("service.version", "1.2.3"),
    ("deployment.environment", "production"),
    ("trace.sampled", "true"),
    ("user.id", "user-123"),
    ("request.id", "req-456"),
    ("session.id", "session-789"),
    ("correlation.id", "corr-012")
  ]
  
  let mut i = 0
  while i < expected_entries.length() {
    let (key, expected_value) = expected_entries[i]
    match baggage.get(key) {
      Some(actual_value) => assert_eq(actual_value, expected_value)
      None => @test.fail("Expected Some value for key: " + key)
    }
    i = i + 1
  }
}

test "baggage_edge_cases" {
  // 测试Baggage的边界情况
  
  // 空字符串键和值
  let baggage1 = Baggage::empty()
    .with_entry("", "")
    .with_entry("empty.value", "")
    .with_entry("", "empty.key")
  
  assert_eq(baggage1.entries.length(), 3)
  
  match baggage1.get("") {
    Some(value) => assert_eq(value, "")
    None => @test.fail("Test failed")
  }
  
  match baggage1.get("empty.value") {
    Some(value) => assert_eq(value, "")
    None => @test.fail("Test failed")
  }
  
  // 特殊字符
  let baggage2 = Baggage::empty()
    .with_entry("special.chars", "!@#$%^&*()")
    .with_entry("unicode.test", "测试中文🚀")
    .with_entry("spaces.key", "key with spaces")
    .with_entry("url.encoded", "key%20with%20encoding")
  
  assert_eq(baggage2.entries.length(), 4)
  
  match baggage2.get("special.chars") {
    Some(value) => assert_eq(value, "!@#$%^&*()")
    None => @test.fail("Test failed")
  }
  
  match baggage2.get("unicode.test") {
    Some(value) => assert_eq(value, "测试中文🚀")
    None => @test.fail("Test failed")
  }
  
  // 长字符串
  let long_key = "a".repeat(1000)
  let long_value = "b".repeat(1000)
  let baggage3 = Baggage::empty().with_entry(long_key, long_value)
  
  assert_eq(baggage3.entries.length(), 1)
  match baggage3.get(long_key) {
    Some(value) => assert_eq(value, long_value)
    None => @test.fail("Test failed")
  }
}

test "context_with_baggage_integration" {
  // 测试Context与Baggage的集成
  
  let ctx = Context::empty()
  let baggage_key = create_key("baggage")
  
  // 创建复杂的baggage字符串
  let baggage_value = "user.id=123,request.id=456,service.name=api"
  let ctx_with_baggage = ctx.with_value(baggage_key, baggage_value)
  
  // 从context中获取baggage
  match ctx_with_baggage.get(baggage_key) {
    Some(stored_baggage) => {
      assert_eq(stored_baggage, "user.id=123,request.id=456,service.name=api")
      
      // 验证baggage内容
      assert_eq(stored_baggage.contains("user.id=123"), true)
      assert_eq(stored_baggage.contains("request.id=456"), true)
      assert_eq(stored_baggage.contains("service.name=api"), true)
    }
    None => @test.fail("Test failed")
  }
  
  // 测试多个context值
  let trace_id_key = create_key("trace_id")
  let span_id_key = create_key("span_id")
  
  let ctx_full = ctx_with_baggage
    .with_value(trace_id_key, "0af7651916cd43dd8448eb211c80319c")
    .with_value(span_id_key, "b7ad6b7169203331")
  
  // 验证所有值都存在
  match ctx_full.get(baggage_key) {
    Some(value) => assert_eq(value, "user.id=123,request.id=456,service.name=api")
    None => @test.fail("Test failed")
  }
  
  match ctx_full.get(trace_id_key) {
    Some(value) => assert_eq(value, "0af7651916cd43dd8448eb211c80319c")
    None => @test.fail("Test failed")
  }
  
  match ctx_full.get(span_id_key) {
    Some(value) => assert_eq(value, "b7ad6b7169203331")
    None => @test.fail("Test failed")
  }
}

test "context_key_creation_and_usage" {
  // 测试ContextKey的创建和使用
  
  // 创建不同类型的键
  let string_key = create_key("string.key")
  let numeric_key = create_key("numeric.key.123")
  let special_key = create_key("special-key_with.dots-and_underscores")
  let unicode_key = create_key("unicode.键.测试")
  
  assert_eq(string_key.name, "string.key")
  assert_eq(numeric_key.name, "numeric.key.123")
  assert_eq(special_key.name, "special-key_with.dots-and_underscores")
  assert_eq(unicode_key.name, "unicode.键.测试")
  
  // 在context中使用这些键
  let ctx = Context::empty()
  
  let ctx1 = ctx.with_value(string_key, "string value")
  let ctx2 = ctx1.with_value(numeric_key, "numeric value 123")
  let ctx3 = ctx2.with_value(special_key, "special value !@#$%")
  let ctx4 = ctx3.with_value(unicode_key, "unicode 值 测试 🚀")
  
  // 验证所有值都能正确获取
  match ctx4.get(string_key) {
    Some(value) => assert_eq(value, "string value")
    None => @test.fail("Test failed")
  }
  
  match ctx4.get(numeric_key) {
    Some(value) => assert_eq(value, "numeric value 123")
    None => @test.fail("Test failed")
  }
  
  match ctx4.get(special_key) {
    Some(value) => assert_eq(value, "special value !@#$%")
    None => @test.fail("Test failed")
  }
  
  match ctx4.get(unicode_key) {
    Some(value) => assert_eq(value, "unicode 值 测试 🚀")
    None => @test.fail("Test failed")
  }
}

test "context_value_overwrite_behavior" {
  // 测试Context值的覆盖行为
  
  let ctx = Context::empty()
  let key = create_key("test.key")
  
  // 设置初始值
  let ctx1 = ctx.with_value(key, "initial_value")
  match ctx1.get(key) {
    Some(value) => assert_eq(value, "initial_value")
    None => @test.fail("Test failed")
  }
  
  // 覆盖值
  let ctx2 = ctx1.with_value(key, "overwritten_value")
  match ctx2.get(key) {
    Some(value) => assert_eq(value, "overwritten_value")
    None => @test.fail("Test failed")
  }
  
  // 再次覆盖
  let ctx3 = ctx2.with_value(key, "final_value")
  match ctx3.get(key) {
    Some(value) => assert_eq(value, "final_value")
    None => @test.fail("Test failed")
  }
  
  // 验证context中值的数量
  assert_eq(ctx3.values.length(), 1) // 应该只有一个值，因为覆盖了相同键的值
}

test "context_multiple_keys_isolation" {
  // 测试Context中多个键的隔离
  
  let ctx = Context::empty()
  let key1 = create_key("key.one")
  let key2 = create_key("key.two")
  let key3 = create_key("key.three")
  
  // 设置多个不同的键值对
  let ctx_final = ctx
    .with_value(key1, "value.one")
    .with_value(key2, "value.two")
    .with_value(key3, "value.three")
  
  // 验证每个键都能获取到正确的值
  match ctx_final.get(key1) {
    Some(value) => assert_eq(value, "value.one")
    None => @test.fail("Test failed")
  }
  
  match ctx_final.get(key2) {
    Some(value) => assert_eq(value, "value.two")
    None => @test.fail("Test failed")
  }
  
  match ctx_final.get(key3) {
    Some(value) => assert_eq(value, "value.three")
    None => @test.fail("Test failed")
  }
  
  // 验证context中值的总数
  assert_eq(ctx_final.values.length(), 3)
}