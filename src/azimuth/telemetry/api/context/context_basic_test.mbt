// Context模块基本测试用例
test "context_basic_operations" {
  let ctx = Context::empty()
  let key = create_key("test_key")
  let ctx_with_value = ctx.with_value(key, "test_value")
  
  let retrieved_value = ctx_with_value.get(key)
  match retrieved_value {
    Some(value) => assert_eq(value, "test_value")
    None => @test.fail("Expected Some(value)")
  }
  
  // 测试不存在的键
  let non_existent_key = create_key("non_existent")
  let non_existent_value = ctx_with_value.get(non_existent_key)
  match non_existent_value {
    Some(_) => @test.fail("Expected None")
    None => assert_eq(true, true)
  }
}

test "baggage_basic_operations" {
  let baggage = Baggage::empty()
  let baggage_with_entry = baggage.with_entry("test_key", "test_value")
  
  let retrieved_value = baggage_with_entry.get("test_key")
  match retrieved_value {
    Some(value) => assert_eq(value, "test_value")
    None => @test.fail("Expected Some(value)")
  }
  
  // 测试不存在的键
  let non_existent_value = baggage_with_entry.get("non_existent")
  match non_existent_value {
    Some(_) => @test.fail("Expected None")
    None => assert_eq(true, true)
  }
}

test "context_key_creation" {
  let key1 = create_key("key1")
  let key2 = create_key("key2")
  
  assert_eq(key1.name, "key1")
  assert_eq(key2.name, "key2")
  if key1.name == key2.name {
    @test.fail("Expected key1.name != key2.name")
  }
}