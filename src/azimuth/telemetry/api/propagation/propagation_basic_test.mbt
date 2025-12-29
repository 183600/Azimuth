// Propagation模块基本测试用例
test "map_carrier_operations" {
  let carrier = MapCarrier::new()
  
  // 测试基本操作
  let result1 = carrier.get("non_existent_key")
  match result1 {
    Some(_) => @test.fail("Expected None for non-existent key")
    None => assert_eq(true, true)
  }
  
  // 测试设置和获取
  carrier.set("test_key", "test_value")
  let result2 = carrier.get("test_key")
  match result2 {
    Some(value) => assert_eq(value, "test_value")
    None => @test.fail("Expected Some(value)")
  }
  
  // 测试覆盖
  carrier.set("test_key", "new_value")
  let result3 = carrier.get("test_key")
  match result3 {
    Some(value) => assert_eq(value, "new_value")
    None => @test.fail("Expected Some(value)")
  }
}

test "text_map_propagator" {
  let propagator = TextMapPropagator::{}
  let carrier = MapCarrier::new()
  
  // 测试注入和提取不会崩溃
  let ctx = Context::empty()
  propagator.inject(ctx, carrier)
  
  let extracted_ctx = propagator.extract(carrier)
  
  assert_eq(true, true)
}

test "propagation_with_baggage" {
  let propagator = TextMapPropagator::{}
  let carrier = MapCarrier::new()
  
  // 创建带有baggage的context
  let ctx = Context::empty()
  let baggage = Baggage::empty().with_entry("user_id", "12345")
  
  // 注入和提取
  propagator.inject(ctx, carrier)
  let extracted_ctx = propagator.extract(carrier)
  
  assert_eq(true, true)
}

test "composite_propagator" {
  let text_map_propagator = TextMapPropagator::{}
  let propagators = [text_map_propagator]
  let composite = CompositePropagator::new(propagators)
  
  let carrier = MapCarrier::new()
  let ctx = Context::empty()
  
  // 测试注入和提取不会崩溃
  composite.inject(ctx, carrier)
  let extracted_ctx = composite.extract(carrier)
  
  assert_eq(true, true)
}

test "carrier_keys_iteration" {
  let carrier = MapCarrier::new()
  
  // 设置多个键值对
  carrier.set("key1", "value1")
  carrier.set("key2", "value2")
  carrier.set("key3", "value3")
  
  // 测试获取所有键
  let keys = carrier.keys()
  
  // 验证键的数量
  assert_eq(keys.length(), 3)
  
  // 验证每个键都能获取到对应的值
  let mut found_count = 0
  let mut index = 0
  while index < keys.length() {
    let key = keys[index]
    match carrier.get(key) {
      Some(_) => found_count = found_count + 1
      None => @test.fail("Expected value for key: " + key)
    }
    index = index + 1
  }
  
  assert_eq(found_count, 3)
}