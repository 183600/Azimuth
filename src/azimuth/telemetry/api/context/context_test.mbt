test "context_multiple_values" {
  let ctx = Context::empty()
  let key1 = create_key("test-key-1")
  let key2 = create_key("test-key-2")
  
  // 测试设置多个值
  let ctx_with_value1 = ctx.with_value(key1, "test-value-1")
  let ctx_with_values = ctx_with_value1.with_value(key2, "test-value-2")
  
  match ctx_with_values.get(key1) {
    Some(value) => assert_eq(value, "test-value-1")
    None => @test.fail("Expected Some(value)")
  }
  
  match ctx_with_values.get(key2) {
    Some(value) => assert_eq(value, "test-value-2")
    None => @test.fail("Expected Some(value)")
  }
}

test "baggage_multiple_entries" {
  let baggage = Baggage::empty()
  
  // 测试添加多个条目
  let baggage1 = baggage.with_entry("user.id", "12345")
  let baggage2 = baggage1.with_entry("session.id", "abcdef")
  let baggage3 = baggage2.with_entry("request.id", "xyz789")
  
  match baggage3.get("user.id") {
    Some(value) => assert_eq(value, "12345")
    None => @test.fail("Expected Some(value)")
  }
  
  match baggage3.get("session.id") {
    Some(value) => assert_eq(value, "abcdef")
    None => @test.fail("Expected Some(value)")
  }
  
  match baggage3.get("request.id") {
    Some(value) => assert_eq(value, "xyz789")
    None => @test.fail("Expected Some(value)")
  }
  
  // 测试不存在的键
  match baggage3.get("non.existent") {
    Some(_) => @test.fail("Expected None")
    None => assert_eq(true, true)
  }
}