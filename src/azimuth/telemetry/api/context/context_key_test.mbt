// ContextKey功能的测试用例
// 测试上下文键的创建和使用

test "context_key_creation_and_usage" {
  // 测试ContextKey的创建和基本使用
  let key1 = create_key("test-key-1")
  let key2 = create_key("test-key-2")
  let key3 = create_key("test-key-1") // 与key1同名
  
  assert_eq(key1.name, "test-key-1")
  assert_eq(key2.name, "test-key-2")
  assert_eq(key3.name, "test-key-1")
  
  // 虽然key1和key3同名，但它们是不同的实例
  // 在实际使用中，应该保持对同一key实例的引用
}

test "context_key_with_various_names" {
  // 测试各种名称的ContextKey
  let simple_key = create_key("simple")
  let underscore_key = create_key("snake_case_key")
  let dot_key = create_key("dotted.key.name")
  let number_key = create_key("key123")
  let mixed_key = create_key("mixed-Style_key.name123")
  let unicode_key = create_key("测试键")
  let empty_key = create_key("")
  
  assert_eq(simple_key.name, "simple")
  assert_eq(underscore_key.name, "snake_case_key")
  assert_eq(dot_key.name, "dotted.key.name")
  assert_eq(number_key.name, "key123")
  assert_eq(mixed_key.name, "mixed-Style_key.name123")
  assert_eq(unicode_key.name, "测试键")
  assert_eq(empty_key.name, "")
}

test "context_operations_with_different_keys" {
  // 测试使用不同键的上下文操作
  let ctx = Context::empty()
  
  let string_key = create_key("string.key")
  let number_key = create_key("number.key")
  let boolean_key = create_key("boolean.key")
  let unicode_key = create_key("unicode.键")
  
  // 设置不同类型的值
  let ctx1 = ctx.with_value(string_key, "string value")
  let ctx2 = ctx1.with_value(number_key, "42")
  let ctx3 = ctx2.with_value(boolean_key, "true")
  let ctx4 = ctx3.with_value(unicode_key, "测试值")
  
  // 获取值并验证
  match ctx4.get(string_key) {
    Some(value) => assert_eq(value, "string value")
    None => @test.fail("Expected Some(value)")
  }
  
  match ctx4.get(number_key) {
    Some(value) => assert_eq(value, "42")
    None => @test.fail("Expected Some(value)")
  }
  
  match ctx4.get(boolean_key) {
    Some(value) => assert_eq(value, "true")
    None => @test.fail("Expected Some(value)")
  }
  
  match ctx4.get(unicode_key) {
    Some(value) => assert_eq(value, "测试值")
    None => @test.fail("Expected Some(value)")
  }
}

test "context_key_value_overwrite" {
  // 测试使用相同键覆盖值
  let ctx = Context::empty()
  let key = create_key("overwrite.key")
  
  // 设置初始值
  let ctx1 = ctx.with_value(key, "initial value")
  match ctx1.get(key) {
    Some(value) => assert_eq(value, "initial value")
    None => @test.fail("Expected Some(value)")
  }
  
  // 覆盖值
  let ctx2 = ctx1.with_value(key, "overwritten value")
  match ctx2.get(key) {
    Some(value) => assert_eq(value, "overwritten value")
    None => @test.fail("Expected Some(value)")
  }
  
  // 再次覆盖
  let ctx3 = ctx2.with_value(key, "final value")
  match ctx3.get(key) {
    Some(value) => assert_eq(value, "final value")
    None => @test.fail("Expected Some(value)")
  }
}

test "context_key_non_existent" {
  // 测试获取不存在的键
  let ctx = Context::empty()
  let existing_key = create_key("existing.key")
  let non_existent_key1 = create_key("non.existent.key1")
  let non_existent_key2 = create_key("non.existent.key2")
  
  // 设置一个值
  let ctx1 = ctx.with_value(existing_key, "value")
  
  // 验证存在的键
  match ctx1.get(existing_key) {
    Some(value) => assert_eq(value, "value")
    None => @test.fail("Expected Some(value)")
  }
  
  // 验证不存在的键
  match ctx1.get(non_existent_key1) {
    Some(_) => @test.fail("Expected None")
    None => assert_eq(true, true)
  }
  
  match ctx1.get(non_existent_key2) {
    Some(_) => @test.fail("Expected None")
    None => assert_eq(true, true)
  }
}

test "context_key_with_special_values" {
  // 测试包含特殊字符的值
  let ctx = Context::empty()
  
  let keys = [
    create_key("empty.string"),
    create_key("spaces"),
    create_key("special.chars"),
    create_key("unicode.value"),
    create_key("json.value"),
    create_key("newlines")
  ]
  
  let values = [
    "",
    "   ",
    "special!@#$%^&*()_+-=[]{}|;':\",./<>?",
    "测试 Unicode 🚀",
    "{\"key\": \"value\", \"number\": 42}",
    "line1\nline2\r\nline3"
  ]
  
  // 设置值
  let mut current_ctx = ctx
  let mut i = 0
  while i < keys.length() {
    current_ctx = current_ctx.with_value(keys[i], values[i])
    i = i + 1
  }
  
  // 验证值
  let mut j = 0
  while j < keys.length() {
    match current_ctx.get(keys[j]) {
      Some(value) => assert_eq(value, values[j])
      None => @test.fail("Expected Some(value)")
    }
    j = j + 1
  }
}

test "context_key_with_long_names_and_values" {
  // 测试长名称和值
  let long_key_name = "this.is.a.very.long.key.name.that.might.be.used.in.real.world.applications.to.identify.specific.context.values"
  let long_value = "this.is.a.very.long.value.that.might.contain.detailed.information.about.the.context.such.as.user.identifiers.session.ids.or.other.metadata.that.needs.to.be.stored.and.retrieved.later"
  
  let ctx = Context::empty()
  let long_key = create_key(long_key_name)
  
  // 设置长值
  let ctx1 = ctx.with_value(long_key, long_value)
  
  // 验证长值
  match ctx1.get(long_key) {
    Some(value) => assert_eq(value, long_value)
    None => @test.fail("Expected Some(value)")
  }
  
  // 验证键名
  assert_eq(long_key.name, long_key_name)
}

test "multiple_context_keys_independence" {
  // 测试多个上下文键的独立性
  let ctx = Context::empty()
  
  let key_a = create_key("key.a")
  let key_b = create_key("key.b")
  let key_c = create_key("key.c")
  
  // 创建不同的上下文分支
  let ctx_a = ctx.with_value(key_a, "value.a")
  let ctx_b = ctx.with_value(key_b, "value.b")
  let ctx_c = ctx.with_value(key_c, "value.c")
  
  // 验证独立性
  match ctx_a.get(key_a) {
    Some(value) => assert_eq(value, "value.a")
    None => @test.fail("Expected Some(value)")
  }
  
  match ctx_a.get(key_b) {
    Some(_) => @test.fail("Expected None")
    None => assert_eq(true, true)
  }
  
  match ctx_b.get(key_b) {
    Some(value) => assert_eq(value, "value.b")
    None => @test.fail("Expected Some(value)")
  }
  
  match ctx_b.get(key_c) {
    Some(_) => @test.fail("Expected None")
    None => assert_eq(true, true)
  }
  
  match ctx_c.get(key_c) {
    Some(value) => assert_eq(value, "value.c")
    None => @test.fail("Expected Some(value)")
  }
  
  match ctx_c.get(key_a) {
    Some(_) => @test.fail("Expected None")
    None => assert_eq(true, true)
  }
}