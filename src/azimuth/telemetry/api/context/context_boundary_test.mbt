test "context_with_nested_values" {
  // 测试嵌套Context值的管理
  
  let base_ctx = Context::empty()
  let key1 = create_key("level1")
  let key2 = create_key("level2")
  let key3 = create_key("level3")
  let key4 = create_key("level4")
  
  // 创建多层嵌套的context
  let ctx1 = base_ctx.with_value(key1, "value1")
  let ctx2 = ctx1.with_value(key2, "value2")
  let ctx3 = ctx2.with_value(key3, "value3")
  let ctx4 = ctx3.with_value(key4, "value4")
  
  // 验证所有值都存在
  match ctx4.get(key1) {
    Some(value) => assert_eq(value, "value1")
    None => @test.fail("Expected Some(value1)")
  }
  
  match ctx4.get(key2) {
    Some(value) => assert_eq(value, "value2")
    None => @test.fail("Expected Some(value2)")
  }
  
  match ctx4.get(key3) {
    Some(value) => assert_eq(value, "value3")
    None => @test.fail("Expected Some(value3)")
  }
  
  match ctx4.get(key4) {
    Some(value) => assert_eq(value, "value4")
    None => @test.fail("Expected Some(value4)")
  }
  
  // 验证原始context不受影响
  match base_ctx.get(key1) {
    Some(_) => @test.fail("Expected None")
    None => assert_eq(true, true)
  }
}

test "baggage_with_complex_values" {
  // 测试Baggage的复杂值处理
  
  let baggage = Baggage::empty()
  
  // 测试包含特殊字符的值
  let baggage1 = baggage.with_entry("special.chars", "!@#$%^&*()")
  let baggage2 = baggage1.with_entry("unicode.value", "测试中文🚀")
  let baggage3 = baggage2.with_entry("url.encoded", "hello%20world")
  let baggage4 = baggage3.with_entry("empty.value", "")
  let baggage5 = baggage4.with_entry("spaces", "   leading and trailing   ")
  
  // 验证特殊字符值
  match baggage5.get("special.chars") {
    Some(value) => assert_eq(value, "!@#$%^&*()")
    None => @test.fail("Expected Some(value)")
  }
  
  // 验证Unicode值
  match baggage5.get("unicode.value") {
    Some(value) => {
      assert_eq(value.contains("测试"), true)
      assert_eq(value.contains("🚀"), true)
    }
    None => @test.fail("Expected Some(value)")
  }
  
  // 验证空值
  match baggage5.get("empty.value") {
    Some(value) => assert_eq(value, "")
    None => @test.fail("Expected Some(value)")
  }
  
  // 验证包含空格的值
  match baggage5.get("spaces") {
    Some(value) => assert_eq(value, "   leading and trailing   ")
    None => @test.fail("Expected Some(value)")
  }
}