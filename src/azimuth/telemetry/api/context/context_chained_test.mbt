// Context链式操作测试用例
test "context_chained_operations_basic" {
  // 测试基本的Context链式操作
  let ctx = Context::empty()
  let key1 = create_key("user.id")
  let key2 = create_key("request.id")
  let key3 = create_key("session.id")
  let key4 = create_key("trace.id")
  let key5 = create_key("correlation.id")
  
  // 链式操作
  let chained_ctx = ctx
    .with_value(key1, "user-123")
    .with_value(key2, "req-456")
    .with_value(key3, "session-789")
    .with_value(key4, "trace-012")
    .with_value(key5, "corr-345")
  
  // 验证所有值都正确设置
  match chained_ctx.get(key1) {
    Some(value) => assert_eq(value, "user-123")
    None => @test.fail("Test failed")
  }
  
  match chained_ctx.get(key2) {
    Some(value) => assert_eq(value, "req-456")
    None => @test.fail("Test failed")
  }
  
  match chained_ctx.get(key3) {
    Some(value) => assert_eq(value, "session-789")
    None => @test.fail("Test failed")
  }
  
  match chained_ctx.get(key4) {
    Some(value) => assert_eq(value, "trace-012")
    None => @test.fail("Test failed")
  }
  
  match chained_ctx.get(key5) {
    Some(value) => assert_eq(value, "corr-345")
    None => @test.fail("Test failed")
  }
}

test "context_chained_operations_with_overwrites" {
  // 测试链式操作中的值覆盖
  let ctx = Context::empty()
  let key1 = create_key("user.id")
  let key2 = create_key("request.id")
  
  // 链式操作，包含覆盖
  let chained_ctx = ctx
    .with_value(key1, "user-123")
    .with_value(key2, "req-456")
    .with_value(key1, "user-789")  // 覆盖user.id
    .with_value(key2, "req-012")   // 覆盖request.id
    .with_value(key1, "user-345")  // 再次覆盖user.id
  
  // 验证最终值
  match chained_ctx.get(key1) {
    Some(value) => assert_eq(value, "user-345")
    None => @test.fail("Test failed")
  }
  
  match chained_ctx.get(key2) {
    Some(value) => assert_eq(value, "req-012")
    None => @test.fail("Test failed")
  }
}

test "context_chained_operations_performance" {
  // 测试链式操作的性能（大量操作）
  let ctx = Context::empty()
  let mut chained_ctx = ctx
  
  // 执行100次链式操作
  let mut i = 0
  while i < 100 {
    let key = create_key("key." + i.to_string())
    chained_ctx = chained_ctx.with_value(key, "value." + i.to_string())
    i = i + 1
  }
  
  // 验证部分值
  match chained_ctx.get(create_key("key.0")) {
    Some(value) => assert_eq(value, "value.0")
    None => @test.fail("Test failed")
  }
  
  match chained_ctx.get(create_key("key.49")) {
    Some(value) => assert_eq(value, "value.49")
    None => @test.fail("Test failed")
  }
  
  match chained_ctx.get(create_key("key.99")) {
    Some(value) => assert_eq(value, "value.99")
    None => @test.fail("Test failed")
  }
}

test "baggage_chained_operations_basic" {
  // 测试基本的Baggage链式操作
  let baggage = Baggage::empty()
  
  // 链式操作
  let chained_baggage = baggage
    .with_entry("user.id", "user-123")
    .with_entry("request.id", "req-456")
    .with_entry("session.id", "session-789")
    .with_entry("trace.id", "trace-012")
    .with_entry("correlation.id", "corr-345")
  
  // 验证所有值都正确设置
  match chained_baggage.get("user.id") {
    Some(value) => assert_eq(value, "user-123")
    None => @test.fail("Test failed")
  }
  
  match chained_baggage.get("request.id") {
    Some(value) => assert_eq(value, "req-456")
    None => @test.fail("Test failed")
  }
  
  match chained_baggage.get("session.id") {
    Some(value) => assert_eq(value, "session-789")
    None => @test.fail("Test failed")
  }
  
  match chained_baggage.get("trace.id") {
    Some(value) => assert_eq(value, "trace-012")
    None => @test.fail("Test failed")
  }
  
  match chained_baggage.get("correlation.id") {
    Some(value) => assert_eq(value, "corr-345")
    None => @test.fail("Test failed")
  }
}

test "baggage_chained_operations_with_overwrites" {
  // 测试Baggage链式操作中的值覆盖
  let baggage = Baggage::empty()
  
  // 链式操作，包含覆盖
  let chained_baggage = baggage
    .with_entry("user.id", "user-123")
    .with_entry("request.id", "req-456")
    .with_entry("user.id", "user-789")  // 覆盖user.id
    .with_entry("request.id", "req-012")   // 覆盖request.id
    .with_entry("user.id", "user-345")  // 再次覆盖user.id
  
  // 验证最终值
  match chained_baggage.get("user.id") {
    Some(value) => assert_eq(value, "user-345")
    None => @test.fail("Test failed")
  }
  
  match chained_baggage.get("request.id") {
    Some(value) => assert_eq(value, "req-012")
    None => @test.fail("Test failed")
  }
}

test "context_and_baggage_combined_chained_operations" {
  // 测试Context和Baggage的组合链式操作
  let ctx = Context::empty()
  let baggage = Baggage::empty()
  
  // Context链式操作
  let ctx_key1 = create_key("trace.id")
  let ctx_key2 = create_key("span.id")
  let ctx_key3 = create_key("parent.span.id")
  
  let chained_ctx = ctx
    .with_value(ctx_key1, "trace-123456")
    .with_value(ctx_key2, "span-789012")
    .with_value(ctx_key3, "parent-345678")
  
  // Baggage链式操作
  let chained_baggage = baggage
    .with_entry("user.id", "user-123")
    .with_entry("request.id", "req-456")
    .with_entry("session.id", "session-789")
  
  // 验证Context中的值
  match chained_ctx.get(ctx_key1) {
    Some(value) => assert_eq(value, "trace-123456")
    None => @test.fail("Test failed")
  }
  
  match chained_ctx.get(ctx_key2) {
    Some(value) => assert_eq(value, "span-789012")
    None => @test.fail("Test failed")
  }
  
  match chained_ctx.get(ctx_key3) {
    Some(value) => assert_eq(value, "parent-345678")
    None => @test.fail("Test failed")
  }
  
  // 验证Baggage中的值
  match chained_baggage.get("user.id") {
    Some(value) => assert_eq(value, "user-123")
    None => @test.fail("Test failed")
  }
  
  match chained_baggage.get("request.id") {
    Some(value) => assert_eq(value, "req-456")
    None => @test.fail("Test failed")
  }
  
  match chained_baggage.get("session.id") {
    Some(value) => assert_eq(value, "session-789")
    None => @test.fail("Test failed")
  }
}

test "context_chained_operations_with_special_values" {
  // 测试包含特殊值的Context链式操作
  let ctx = Context::empty()
  
  // 链式操作，包含特殊值
  let chained_ctx = ctx
    .with_value(create_key("empty.value"), "")
    .with_value(create_key("special.chars"), "!@#$%^&*()_+-={}[]|\\:;\"'<>?,./")
    .with_value(create_key("unicode.value"), "Unicode测试🚀")
    .with_value(create_key("long.value"), "This is a very long string that contains many words and characters to test if the context can handle long values properly without any issues or truncation or other problems that might occur with string handling in the implementation.")
    .with_value(create_key("numeric.value"), "1234567890")
    .with_value(create_key("json.value"), "{\"key\":\"value\",\"array\":[1,2,3],\"nested\":{\"prop\":\"val\"}}")
    .with_value(create_key("url.value"), "https://example.com/path?query=value&param2=value2#fragment")
    .with_value(create_key("newline.value"), "line1\nline2\rline3\r\nline4")
    .with_value(create_key("tab.value"), "col1\tcol2\tcol3")
    .with_value(create_key("spaces.value"), "   leading and trailing spaces   ")
  
  // 验证特殊值
  match chained_ctx.get(create_key("empty.value")) {
    Some(value) => assert_eq(value, "")
    None => @test.fail("Test failed")
  }
  
  match chained_ctx.get(create_key("special.chars")) {
    Some(value) => assert_eq(value, "!@#$%^&*()_+-={}[]|\\:;\"'<>?,./")
    None => @test.fail("Test failed")
  }
  
  match chained_ctx.get(create_key("unicode.value")) {
    Some(value) => assert_eq(value, "Unicode测试🚀")
    None => @test.fail("Test failed")
  }
  
  match chained_ctx.get(create_key("json.value")) {
    Some(value) => assert_eq(value, "{\"key\":\"value\",\"array\":[1,2,3],\"nested\":{\"prop\":\"val\"}}")
    None => @test.fail("Test failed")
  }
  
  match chained_ctx.get(create_key("newline.value")) {
    Some(value) => assert_eq(value, "line1\nline2\rline3\r\nline4")
    None => @test.fail("Test failed")
  }
}

test "baggage_chained_operations_with_special_keys" {
  // 测试包含特殊键的Baggage链式操作
  let baggage = Baggage::empty()
  
  // 链式操作，包含特殊键
  let chained_baggage = baggage
    .with_entry("", "empty.key.value")
    .with_entry("special.key.with.dots", "dots.value")
    .with_entry("key with spaces", "spaces.value")
    .with_entry("special-key-with-dashes", "dashes.value")
    .with_entry("special_key_with_underscores", "underscores.value")
    .with_entry("key/with/slashes", "slashes.value")
    .with_entry("key.with.symbols!@#$%", "symbols.value")
    .with_entry("Unicode键测试🚀", "unicode.key.value")
  
  // 验证特殊键
  match chained_baggage.get("") {
    Some(value) => assert_eq(value, "empty.key.value")
    None => @test.fail("Test failed")
  }
  
  match chained_baggage.get("special.key.with.dots") {
    Some(value) => assert_eq(value, "dots.value")
    None => @test.fail("Test failed")
  }
  
  match chained_baggage.get("key with spaces") {
    Some(value) => assert_eq(value, "spaces.value")
    None => @test.fail("Test failed")
  }
  
  match chained_baggage.get("Unicode键测试🚀") {
    Some(value) => assert_eq(value, "unicode.key.value")
    None => @test.fail("Test failed")
  }
}