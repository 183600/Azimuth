test "context_edge_cases" {
  // 测试Context的边界情况
  
  // 测试空Context
  let empty_ctx = Context::empty()
  let empty_key = create_key("")
  
  match empty_ctx.get(empty_key) {
    Some(_) => @test.fail("Expected None for empty context")
    None => assert_eq(true, true)
  }
  
  // 测试大值
  let large_value = "这是一个很长的值，用来测试Context在处理大值时的行为。".repeat(100)
  let ctx_with_large = empty_ctx.with_value(create_key("large.key"), large_value)
  
  match ctx_with_large.get(create_key("large.key")) {
    Some(value) => assert_eq(value.length(), large_value.length())
    None => @test.fail("Expected Some(value)")
  }
  
  // 测试特殊字符键
  let special_key = create_key("special.key!@#$%^&*()")
  let special_value = "special_value!@#$%^&*()"
  let ctx_with_special = empty_ctx.with_value(special_key, special_value)
  
  match ctx_with_special.get(special_key) {
    Some(value) => assert_eq(value, special_value)
    None => @test.fail("Expected Some(value)")
  }
  
  // 测试Unicode键和值
  let unicode_key = create_key("测试键🚀")
  let unicode_value = "测试值🎉"
  let ctx_with_unicode = empty_ctx.with_value(unicode_key, unicode_value)
  
  match ctx_with_unicode.get(unicode_key) {
    Some(value) => assert_eq(value, unicode_value)
    None => @test.fail("Expected Some(value)")
  }
  
  // 测试覆盖值
  let ctx1 = empty_ctx.with_value(create_key("test.key"), "value1")
  let ctx2 = ctx1.with_value(create_key("test.key"), "value2")
  
  match ctx2.get(create_key("test.key")) {
    Some(value) => assert_eq(value, "value2")
    None => @test.fail("Expected Some(value)")
  }
  
  // 测试大量键值对
  let mut ctx_multi = empty_ctx
  let mut i = 0
  while i < 50 {
    ctx_multi = ctx_multi.with_value(create_key("key." + i.to_string()), "value." + i.to_string())
    i = i + 1
  }
  
  // 验证一些键值对
  match ctx_multi.get(create_key("key.0")) {
    Some(value) => assert_eq(value, "value.0")
    None => @test.fail("Expected Some(value)")
  }
  
  match ctx_multi.get(create_key("key.49")) {
    Some(value) => assert_eq(value, "value.49")
    None => @test.fail("Expected Some(value)")
  }
  
  match ctx_multi.get(create_key("key.50")) {
    Some(_) => @test.fail("Expected None for non-existent key")
    None => assert_eq(true, true)
  }
}