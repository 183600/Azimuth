// Context模块复杂场景测试用例
test "context_multiple_values_and_overrides" {
  let ctx = Context::empty()
  let key1 = create_key("user.id")
  let key2 = create_key("request.id")
  let key3 = create_key("trace.id")
  
  // 添加多个值
  let ctx1 = ctx.with_value(key1, "user123")
  let ctx2 = ctx1.with_value(key2, "req456")
  let ctx3 = ctx2.with_value(key3, "trace789")
  
  // 验证所有值都存在
  match ctx3.get(key1) {
    Some(value) => assert_eq(value, "user123")
    None => @test.fail("Expected user.id to be set")
  }
  
  match ctx3.get(key2) {
    Some(value) => assert_eq(value, "req456")
    None => @test.fail("Expected request.id to be set")
  }
  
  match ctx3.get(key3) {
    Some(value) => assert_eq(value, "trace789")
    None => @test.fail("Expected trace.id to be set")
  }
  
  // 测试覆盖值
  let ctx4 = ctx3.with_value(key1, "user456")
  match ctx4.get(key1) {
    Some(value) => assert_eq(value, "user456")
    None => @test.fail("Expected user.id to be overridden")
  }
  
  // 验证其他值未被影响
  match ctx4.get(key2) {
    Some(value) => assert_eq(value, "req456")
    None => @test.fail("Expected request.id to remain unchanged")
  }
}

test "context_with_special_characters" {
  let ctx = Context::empty()
  
  // 测试特殊字符键和值
  let special_key = create_key("special/key/with/special.chars")
  let ctx1 = ctx.with_value(special_key, "特殊值!@#$%^&*()")
  
  match ctx1.get(special_key) {
    Some(value) => assert_eq(value, "特殊值!@#$%^&*()")
    None => @test.fail("Expected special key to be set")
  }
  
  // 测试Unicode字符
  let unicode_key = create_key("Unicode键测试🚀")
  let ctx2 = ctx1.with_value(unicode_key, "Unicode值测试🌟")
  
  match ctx2.get(unicode_key) {
    Some(value) => assert_eq(value, "Unicode值测试🌟")
    None => @test.fail("Expected Unicode key to be set")
  }
  
  // 测试空字符串
  let empty_key = create_key("")
  let ctx3 = ctx2.with_value(empty_key, "")
  
  match ctx3.get(empty_key) {
    Some(value) => assert_eq(value, "")
    None => @test.fail("Expected empty key to be set")
  }
}

test "context_nonexistent_keys" {
  let ctx = Context::empty()
  let existing_key = create_key("existing.key")
  let nonexistent_key1 = create_key("nonexistent.key")
  let nonexistent_key2 = create_key("another.nonexistent.key")
  
  // 只设置一个键
  let ctx1 = ctx.with_value(existing_key, "value")
  
  // 验证存在的键
  match ctx1.get(existing_key) {
    Some(value) => assert_eq(value, "value")
    None => @test.fail("Expected existing.key to be set")
  }
  
  // 验证不存在的键返回None
  match ctx1.get(nonexistent_key1) {
    Some(_) => @test.fail("Expected nonexistent.key to return None")
    None => assert_eq(true, true)
  }
  
  match ctx1.get(nonexistent_key2) {
    Some(_) => @test.fail("Expected another.nonexistent.key to return None")
    None => assert_eq(true, true)
  }
}

test "baggage_complex_operations" {
  let baggage = Baggage::empty()
  
  // 添加多个条目
  let baggage1 = baggage.with_entry("user.id", "user123")
  let baggage2 = baggage1.with_entry("request.id", "req456")
  let baggage3 = baggage2.with_entry("trace.id", "trace789")
  
  // 验证所有条目
  match baggage3.get("user.id") {
    Some(value) => assert_eq(value, "user123")
    None => @test.fail("Expected user.id to be set")
  }
  
  match baggage3.get("request.id") {
    Some(value) => assert_eq(value, "req456")
    None => @test.fail("Expected request.id to be set")
  }
  
  match baggage3.get("trace.id") {
    Some(value) => assert_eq(value, "trace789")
    None => @test.fail("Expected trace.id to be set")
  }
  
  // 测试覆盖条目
  let baggage4 = baggage3.with_entry("user.id", "user456")
  match baggage4.get("user.id") {
    Some(value) => assert_eq(value, "user456")
    None => @test.fail("Expected user.id to be overridden")
  }
  
  // 验证其他条目未被影响
  match baggage4.get("request.id") {
    Some(value) => assert_eq(value, "req456")
    None => @test.fail("Expected request.id to remain unchanged")
  }
}

test "baggage_with_special_characters" {
  let baggage = Baggage::empty()
  
  // 测试特殊字符键和值
  let baggage1 = baggage.with_entry("special/key/with/special.chars", "特殊值!@#$%^&*()")
  
  match baggage1.get("special/key/with/special.chars") {
    Some(value) => assert_eq(value, "特殊值!@#$%^&*()")
    None => @test.fail("Expected special key to be set")
  }
  
  // 测试Unicode字符
  let baggage2 = baggage1.with_entry("Unicode键测试🚀", "Unicode值测试🌟")
  
  match baggage2.get("Unicode键测试🚀") {
    Some(value) => assert_eq(value, "Unicode值测试🌟")
    None => @test.fail("Expected Unicode key to be set")
  }
  
  // 测试空字符串
  let baggage3 = baggage2.with_entry("", "")
  
  match baggage3.get("") {
    Some(value) => assert_eq(value, "")
    None => @test.fail("Expected empty key to be set")
  }
}

test "context_and_baggage_integration" {
  let ctx = Context::empty()
  let baggage = Baggage::empty()
  
  // 在Context中设置值
  let ctx_key = create_key("ctx.value")
  let ctx1 = ctx.with_value(ctx_key, "context_value")
  
  // 在Baggage中设置值
  let baggage1 = baggage.with_entry("baggage.value", "baggage_value")
  
  // 验证它们是独立的
  match ctx1.get(ctx_key) {
    Some(value) => assert_eq(value, "context_value")
    None => @test.fail("Expected context value to be set")
  }
  
  match baggage1.get("baggage.value") {
    Some(value) => assert_eq(value, "baggage_value")
    None => @test.fail("Expected baggage value to be set")
  }
  
  // 验证Context中无法访问Baggage值
  let baggage_key = create_key("baggage.value")
  match ctx1.get(baggage_key) {
    Some(_) => @test.fail("Expected baggage value to not be accessible from context")
    None => assert_eq(true, true)
  }
}

test "context_key_collisions" {
  let ctx = Context::empty()
  
  // 创建相同名称的键
  let key1 = create_key("same.name")
  let key2 = create_key("same.name")
  
  // 使用第一个键设置值
  let ctx1 = ctx.with_value(key1, "value1")
  
  // 使用第二个键设置值
  let ctx2 = ctx1.with_value(key2, "value2")
  
  // 验证值被覆盖（因为键名称相同）
  match ctx2.get(key1) {
    Some(value) => assert_eq(value, "value2")
    None => @test.fail("Expected value to be overridden")
  }
  
  match ctx2.get(key2) {
    Some(value) => assert_eq(value, "value2")
    None => @test.fail("Expected value to be overridden")
  }
}

test "context_chained_operations" {
  let ctx = Context::empty()
  
  // 链式操作
  let ctx1 = ctx
    .with_value(create_key("step1"), "value1")
    .with_value(create_key("step2"), "value2")
    .with_value(create_key("step3"), "value3")
    .with_value(create_key("step4"), "value4")
    .with_value(create_key("step5"), "value5")
  
  // 验证所有值都存在
  match ctx1.get(create_key("step1")) {
    Some(value) => assert_eq(value, "value1")
    None => @test.fail("Expected step1 to be set")
  }
  
  match ctx1.get(create_key("step2")) {
    Some(value) => assert_eq(value, "value2")
    None => @test.fail("Expected step2 to be set")
  }
  
  match ctx1.get(create_key("step3")) {
    Some(value) => assert_eq(value, "value3")
    None => @test.fail("Expected step3 to be set")
  }
  
  match ctx1.get(create_key("step4")) {
    Some(value) => assert_eq(value, "value4")
    None => @test.fail("Expected step4 to be set")
  }
  
  match ctx1.get(create_key("step5")) {
    Some(value) => assert_eq(value, "value5")
    None => @test.fail("Expected step5 to be set")
  }
}