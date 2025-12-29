// Context模块高级测试用例
test "context_complex_operations" {
  let base_context = Context::empty()
  let key1 = create_key("user.id")
  let key2 = create_key("request.id")
  let key3 = create_key("session.id")
  
  // 链式操作测试
  let context1 = base_context.with_value(key1, "user-12345")
  let context2 = context1.with_value(key2, "req-67890")
  let context3 = context2.with_value(key3, "sess-abcdef")
  
  // 验证所有值都存在
  match context3.get(key1) {
    Some(value) => assert_eq(value, "user-12345")
    None => @test.fail("Test failed")
  }
  
  match context3.get(key2) {
    Some(value) => assert_eq(value, "req-67890")
    None => @test.fail("Test failed")
  }
  
  match context3.get(key3) {
    Some(value) => assert_eq(value, "sess-abcdef")
    None => @test.fail("Test failed")
  }
  
  // 测试不存在的键
  let missing_key = create_key("missing.key")
  match context3.get(missing_key) {
    Some(_) => @test.fail("Test failed")
    None => assert_eq(true, true)
  }
}

test "context_with_unicode_and_special_characters" {
  let context = Context::empty()
  
  // 测试Unicode键值
  let unicode_key = create_key("用户ID")
  let context_with_unicode = context.with_value(unicode_key, "用户-12345")
  
  match context_with_unicode.get(unicode_key) {
    Some(value) => assert_eq(value, "用户-12345")
    None => @test.fail("Test failed")
  }
  
  // 测试特殊字符键值
  let special_key = create_key("special!@#$%^&*()")
  let context_with_special = context_with_unicode.with_value(special_key, "value!@#$%^&*()")
  
  match context_with_special.get(special_key) {
    Some(value) => assert_eq(value, "value!@#$%^&*()")
    None => @test.fail("Test failed")
  }
  
  // 测试emoji
  let emoji_key = create_key("🚀rocket")
  let context_with_emoji = context_with_special.with_value(emoji_key, "🔥fire")
  
  match context_with_emoji.get(emoji_key) {
    Some(value) => assert_eq(value, "🔥fire")
    None => @test.fail("Test failed")
  }
  
  // 测试空字符串
  let empty_key = create_key("")
  let context_with_empty = context_with_emoji.with_value(empty_key, "")
  
  match context_with_empty.get(empty_key) {
    Some(value) => assert_eq(value, "")
    None => @test.fail("Test failed")
  }
}

test "baggage_complex_operations" {
  let base_baggage = Baggage::empty()
  
  // 添加多个条目
  let baggage1 = base_baggage.with_entry("user.id", "user-12345")
  let baggage2 = baggage1.with_entry("request.id", "req-67890")
  let baggage3 = baggage2.with_entry("session.id", "sess-abcdef")
  let baggage4 = baggage3.with_entry("trace.id", "trace-123456789")
  let baggage5 = baggage4.with_entry("service.name", "auth-service")
  
  // 验证所有条目
  match baggage5.get("user.id") {
    Some(value) => assert_eq(value, "user-12345")
    None => @test.fail("Test failed")
  }
  
  match baggage5.get("request.id") {
    Some(value) => assert_eq(value, "req-67890")
    None => @test.fail("Test failed")
  }
  
  match baggage5.get("session.id") {
    Some(value) => assert_eq(value, "sess-abcdef")
    None => @test.fail("Test failed")
  }
  
  match baggage5.get("trace.id") {
    Some(value) => assert_eq(value, "trace-123456789")
    None => @test.fail("Test failed")
  }
  
  match baggage5.get("service.name") {
    Some(value) => assert_eq(value, "auth-service")
    None => @test.fail("Test failed")
  }
  
  // 测试不存在的键
  match baggage5.get("missing.key") {
    Some(_) => @test.fail("Test failed")
    None => assert_eq(true, true)
  }
}

test "context_and_baggage_integration" {
  let context = Context::empty()
  let baggage = Baggage::empty()
  
  // 在Context中存储上下文信息
  let trace_key = create_key("trace.id")
  let span_key = create_key("span.id")
  
  let context_with_trace = context.with_value(trace_key, "trace-123456789")
  let context_with_span = context_with_trace.with_value(span_key, "span-987654321")
  
  // 在Baggage中存储传播信息
  let baggage_with_user = baggage.with_entry("user.id", "user-12345")
  let baggage_with_session = baggage_with_user.with_entry("session.id", "sess-abcdef")
  
  // 验证Context和Baggage的独立性
  match context_with_span.get(trace_key) {
    Some(value) => assert_eq(value, "trace-123456789")
    None => @test.fail("Test failed")
  }
  
  match baggage_with_session.get("user.id") {
    Some(value) => assert_eq(value, "user-12345")
    None => @test.fail("Test failed")
  }
  
  // 测试复杂场景：Context存储Baggage信息
  let baggage_key = create_key("baggage")
  let context_with_baggage = context_with_span.with_value(
    baggage_key, 
    "user.id=user-12345,session.id=sess-abcdef"
  )
  
  match context_with_baggage.get(baggage_key) {
    Some(value) => {
      assert_eq(value, "user.id=user-12345,session.id=sess-abcdef")
      
      // 模拟解析baggage字符串
      let parts = value",".split_to_string()
      assert_eq(parts.length(), 2)
      assert_eq(parts[0], "user.id=user-12345")
      assert_eq(parts[1], "session.id=sess-abcdef")
    }
    None => @test.fail("Test failed")
  }
}

test "context_edge_cases_and_boundary_conditions" {
  // 测试边界条件
  
  // 空Context操作
  let empty_context = Context::empty()
  let empty_key = create_key("")
  
  let context_with_empty_key = empty_context.with_value(empty_key, "")
  match context_with_empty_key.get(empty_key) {
    Some(value) => assert_eq(value, "")
    None => @test.fail("Test failed")
  }
  
  // 长字符串测试
  let long_string = "a".repeat(10000)
  let long_key = create_key("long.key")
  
  let context_with_long = context_with_empty_key.with_value(long_key, long_string)
  match context_with_long.get(long_key) {
    Some(value) => assert_eq(value, long_string)
    None => @test.fail("Test failed")
  }
  
  // 特殊字符测试
  let special_chars = "!@#$%^&*()_+-=[]{}|;':\",./<>?~`"
  let special_key = create_key("special.chars")
  
  let context_with_special = context_with_long.with_value(special_key, special_chars)
  match context_with_special.get(special_key) {
    Some(value) => assert_eq(value, special_chars)
    None => @test.fail("Test failed")
  }
  
  // Unicode测试
  let unicode_string = "Unicode测试字符串🚀🔥💯"
  let unicode_key = create_key("unicode.test")
  
  let context_with_unicode = context_with_special.with_value(unicode_key, unicode_string)
  match context_with_unicode.get(unicode_key) {
    Some(value) => assert_eq(value, unicode_string)
    None => @test.fail("Test failed")
  }
}

test "baggage_edge_cases_and_boundary_conditions" {
  // 测试Baggage的边界条件
  
  // 空Baggage操作
  let empty_baggage = Baggage::empty()
  
  let baggage_with_empty_key = empty_baggage.with_entry("", "")
  match baggage_with_empty_key.get("") {
    Some(value) => assert_eq(value, "")
    None => @test.fail("Test failed")
  }
  
  // 长字符串测试
  let long_key = "a".repeat(1000)
  let long_value = "b".repeat(1000)
  
  let baggage_with_long = baggage_with_empty_key.with_entry(long_key, long_value)
  match baggage_with_long.get(long_key) {
    Some(value) => assert_eq(value, long_value)
    None => @test.fail("Test failed")
  }
  
  // 特殊字符测试
  let special_key = "!@#$%^&*()_+-=[]{}|;':\",./<>?"
  let special_value = "!@#$%^&*()_+-=[]{}|;':\",./<>?"
  
  let baggage_with_special = baggage_with_long.with_entry(special_key, special_value)
  match baggage_with_special.get(special_key) {
    Some(value) => assert_eq(value, special_value)
    None => @test.fail("Test failed")
  }
  
  // Unicode测试
  let unicode_key = "Unicode键🚀"
  let unicode_value = "Unicode值🔥"
  
  let baggage_with_unicode = baggage_with_special.with_entry(unicode_key, unicode_value)
  match baggage_with_unicode.get(unicode_key) {
    Some(value) => assert_eq(value, unicode_value)
    None => @test.fail("Test failed")
  }
  
  // 包含等号和逗号的值（这些是baggage的特殊字符）
  let complex_key = "complex.key"
  let complex_value = "value=with=equals,and,commas"
  
  let baggage_with_complex = baggage_with_unicode.with_entry(complex_key, complex_value)
  match baggage_with_complex.get(complex_key) {
    Some(value) => assert_eq(value, complex_value)
    None => @test.fail("Test failed")
  }
}

test "context_performance_and_large_scale" {
  // 测试大规模Context操作的性能
  
  let base_context = Context::empty()
  
  // 创建包含大量键值对的Context
  let mut context = base_context
  let mut i = 0
  while i < 1000 {
    let key = create_key("key" + i.to_string())
    context = context.with_value(key, "value" + i.to_string())
    i = i + 1
  }
  
  // 验证随机访问
  match context.get(create_key("key0")) {
    Some(value) => assert_eq(value, "value0")
    None => @test.fail("Test failed")
  }
  
  match context.get(create_key("key500")) {
    Some(value) => assert_eq(value, "value500")
    None => @test.fail("Test failed")
  }
  
  match context.get(create_key("key999")) {
    Some(value) => assert_eq(value, "value999")
    None => @test.fail("Test failed")
  }
  
  // 验证不存在的键
  match context.get(create_key("key1000")) {
    Some(_) => @test.fail("Test failed")
    None => assert_eq(true, true)
  }
}

test "baggage_performance_and_large_scale" {
  // 测试大规模Baggage操作的性能
  
  let base_baggage = Baggage::empty()
  
  // 创建包含大量条目的Baggage
  let mut baggage = base_baggage
  let mut i = 0
  while i < 1000 {
    baggage = baggage.with_entry("key" + i.to_string(), "value" + i.to_string())
    i = i + 1
  }
  
  // 验证随机访问
  match baggage.get("key0") {
    Some(value) => assert_eq(value, "value0")
    None => @test.fail("Test failed")
  }
  
  match baggage.get("key500") {
    Some(value) => assert_eq(value, "value500")
    None => @test.fail("Test failed")
  }
  
  match baggage.get("key999") {
    Some(value) => assert_eq(value, "value999")
    None => @test.fail("Test failed")
  }
  
  // 验证不存在的键
  match baggage.get("key1000") {
    Some(_) => @test.fail("Test failed")
    None => assert_eq(true, true)
  }
}