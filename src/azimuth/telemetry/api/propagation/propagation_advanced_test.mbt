// Propagation API高级测试用例
test "composite_propagator_multiple_injections" {
  // 测试CompositePropagator的多个注入器
  let trace_propagator = W3CTraceContextPropagator::{}
  let baggage_propagator = W3CBaggagePropagator::{}
  
  let composite = CompositePropagator::new([trace_propagator, baggage_propagator])
  
  let carrier = MapCarrier::new()
  let ctx = Context::empty()
  
  // 执行注入
  composite.inject(ctx, carrier)
  
  // 验证carrier包含了所有必要的键
  let keys = carrier.keys()
  assert_eq(keys.length(), 2)
  
  // 验证traceparent header存在
  match carrier.get("traceparent") {
    Some(trace_parent) => {
      assert_eq(trace_parent, "00-0af7651916cd43dd8448eb211c80319c-b7ad6b7169203331-01")
    }
    None => @test.fail("Expected traceparent header")
  }
  
  // 验证baggage header存在
  match carrier.get("baggage") {
    Some(baggage) => {
      assert_eq(baggage, "key1=value1,key2=value2")
    }
    None => @test.fail("Expected baggage header")
  }
}

test "map_carrier_complex_operations" {
  // 测试MapCarrier的复杂操作
  let initial_data = [
    ("existing.key", "existing.value"),
    ("another.key", "another.value"),
    ("traceparent", "00-0af7651916cd43dd8448eb211c80319c-b7ad6b7169203331-01"),
    ("baggage", "key1=value1,key2=value2")
  ]
  
  let carrier = MapCarrier::from_map(initial_data)
  
  // 测试获取所有键
  let keys = carrier.keys()
  assert_eq(keys.length(), 4)
  
  // 验证所有键都存在
  match carrier.get("existing.key") {
    Some(value) => assert_eq(value, "existing.value")
    None => @test.fail("Expected existing.key")
  }
  
  match carrier.get("another.key") {
    Some(value) => assert_eq(value, "another.value")
    None => @test.fail("Expected another.key")
  }
  
  match carrier.get("traceparent") {
    Some(value) => assert_eq(value, "00-0af7651916cd43dd8448eb211c80319c-b7ad6b7169203331-01")
    None => @test.fail("Expected traceparent")
  }
  
  match carrier.get("baggage") {
    Some(value) => assert_eq(value, "key1=value1,key2=value2")
    None => @test.fail("Expected baggage")
  }
  
  // 测试获取不存在的键
  match carrier.get("missing.key") {
    Some(_) => @test.fail("Expected None for missing key")
    None => assert_eq(true, true)
  }
  
  // 测试空键
  match carrier.get("") {
    Some(_) => @test.fail("Expected None for empty key")
    None => assert_eq(true, true)
  }
  
  // 测试特殊字符键
  let special_data = [
    ("special!@#$%^&*()", "special.value"),
    ("unicode.测试", "unicode.value"),
    ("space key", "space.value"),
    ("dot.separated.key", "dot.value")
  ]
  
  let special_carrier = MapCarrier::from_map(special_data)
  
  match special_carrier.get("special!@#$%^&*()") {
    Some(value) => assert_eq(value, "special.value")
    None => @test.fail("Expected special character key")
  }
  
  match special_carrier.get("unicode.测试") {
    Some(value) => assert_eq(value, "unicode.value")
    None => @test.fail("Expected unicode key")
  }
  
  match special_carrier.get("space key") {
    Some(value) => assert_eq(value, "space.value")
    None => @test.fail("Expected space key")
  }
  
  match special_carrier.get("dot.separated.key") {
    Some(value) => assert_eq(value, "dot.value")
    None => @test.fail("Expected dot separated key")
  }
  
  // 测试转换为map
  let converted_map = special_carrier.to_map()
  assert_eq(converted_map.length(), 4)
}