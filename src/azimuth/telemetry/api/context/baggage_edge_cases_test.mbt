test "baggage_edge_cases" {
  // 测试Baggage的边界情况
  
  let empty_baggage = Baggage::empty()
  
  // 测试空Baggage的操作
  match empty_baggage.get("non.existent") {
    Some(_) => @test.fail("Expected None for empty baggage")
    None => assert_eq(true, true)
  }
  
  // 测试大值
  let large_value = "这是一个很长的值，用来测试Baggage在处理大值时的行为。".repeat(50)
  let baggage_with_large = empty_baggage.with_entry("large.key", large_value)
  
  match baggage_with_large.get("large.key") {
    Some(value) => assert_eq(value.length(), large_value.length())
    None => @test.fail("Expected Some(value)")
  }
  
  // 测试特殊字符键和值
  let special_key = "special.key!@#$%^&*()"
  let special_value = "special_value!@#$%^&*()"
  let baggage_with_special = empty_baggage.with_entry(special_key, special_value)
  
  match baggage_with_special.get(special_key) {
    Some(value) => assert_eq(value, special_value)
    None => @test.fail("Expected Some(value)")
  }
  
  // 测试Unicode键和值
  let unicode_key = "测试键🚀"
  let unicode_value = "测试值🎉"
  let baggage_with_unicode = empty_baggage.with_entry(unicode_key, unicode_value)
  
  match baggage_with_unicode.get(unicode_key) {
    Some(value) => assert_eq(value, unicode_value)
    None => @test.fail("Expected Some(value)")
  }
  
  // 测试覆盖值
  let baggage1 = empty_baggage.with_entry("test.key", "value1")
  let baggage2 = baggage1.with_entry("test.key", "value2")
  
  match baggage2.get("test.key") {
    Some(value) => assert_eq(value, "value2")
    None => @test.fail("Expected Some(value)")
  }
  
  // 测试空键和空值
  let baggage_empty_key = empty_baggage.with_entry("", "empty.key.value")
  match baggage_empty_key.get("") {
    Some(value) => assert_eq(value, "empty.key.value")
    None => @test.fail("Expected Some(value)")
  }
  
  let baggage_empty_value = empty_baggage.with_entry("empty.value.key", "")
  match baggage_empty_value.get("empty.value.key") {
    Some(value) => assert_eq(value, "")
    None => @test.fail("Expected Some(value)")
  }
  
  // 测试包含空格的键和值
  let baggage_spaces = empty_baggage.with_entry("key with spaces", "value with spaces")
  match baggage_spaces.get("key with spaces") {
    Some(value) => assert_eq(value, "value with spaces")
    None => @test.fail("Expected Some(value)")
  }
  
  // 测试数字键（虽然类型是String，但可以包含数字）
  let baggage_numeric_key = empty_baggage.with_entry("123", "numeric.key.value")
  match baggage_numeric_key.get("123") {
    Some(value) => assert_eq(value, "numeric.key.value")
    None => @test.fail("Expected Some(value)")
  }
  
  // 测试大量条目
  let mut baggage_multi = empty_baggage
  let mut i = 0
  while i < 30 {
    baggage_multi = baggage_multi.with_entry("key." + i.to_string(), "value." + i.to_string())
    i = i + 1
  }
  
  // 验证一些条目
  match baggage_multi.get("key.0") {
    Some(value) => assert_eq(value, "value.0")
    None => @test.fail("Expected Some(value)")
  }
  
  match baggage_multi.get("key.29") {
    Some(value) => assert_eq(value, "value.29")
    None => @test.fail("Expected Some(value)")
  }
  
  match baggage_multi.get("key.30") {
    Some(_) => @test.fail("Expected None for non-existent key")
    None => assert_eq(true, true)
  }
  
  // 测试包含特殊HTTP头的键值对
  let baggage_headers = empty_baggage
    .with_entry("user-id", "12345")
    .with_entry("session-id", "abcdef123456")
    .with_entry("request-id", "req-789")
    .with_entry("trace-id", "trace-abc123")
    .with_entry("correlation-id", "corr-def456")
  
  match baggage_headers.get("user-id") {
    Some(value) => assert_eq(value, "12345")
    None => @test.fail("Expected Some(value)")
  }
  
  match baggage_headers.get("correlation-id") {
    Some(value) => assert_eq(value, "corr-def456")
    None => @test.fail("Expected Some(value)")
  }
}