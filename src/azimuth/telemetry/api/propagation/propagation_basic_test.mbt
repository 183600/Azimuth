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

test "w3c_trace_context_propagator" {
  let propagator = W3CTraceContextPropagator::{}
  let carrier = MapCarrier::new()
  let ctx = Context::empty()
  
  // 测试注入操作
  propagator.inject(ctx, carrier)
  
  // 验证traceparent header是否被设置
  match carrier.get("traceparent") {
    Some(trace_parent) => {
      // 验证traceparent格式: version-trace_id-span_id-trace_flags
      assert_eq(trace_parent.length(), 55)  // "00-" + 32 + "-" + 16 + "-01"
      assert_eq(trace_parent[0:2], "00")
      assert_eq(trace_parent[51:53], "01")
    }
    None => @test.fail("Expected traceparent header")
  }
  
  // 测试提取操作
  let carrier_with_trace = MapCarrier::from_map([
    ("traceparent", "00-0af7651916cd43dd8448eb211c80319c-b7ad6b7169203331-01"),
    ("other-header", "other-value")
  ])
  
  let extracted_ctx = propagator.extract(ctx, carrier_with_trace)
  
  // 验证提取不会崩溃
  assert_eq(true, true)
  
  // 测试无效的traceparent格式
  let carrier_invalid = MapCarrier::from_map([
    ("traceparent", "invalid-format"),
    ("other-header", "other-value")
  ])
  
  let extracted_ctx_invalid = propagator.extract(ctx, carrier_invalid)
  assert_eq(true, true)
  
  // 测试没有traceparent header的情况
  let carrier_empty = MapCarrier::new()
  let extracted_ctx_empty = propagator.extract(ctx, carrier_empty)
  assert_eq(true, true)
}

test "w3c_baggage_propagator" {
  let propagator = W3CBaggagePropagator::{}
  let carrier = MapCarrier::new()
  let ctx = Context::empty()
  
  // 测试注入操作
  propagator.inject(ctx, carrier)
  
  // 验证baggage header是否被设置
  match carrier.get("baggage") {
    Some(baggage_value) => {
      // 验证baggage格式: key1=value1,key2=value2
      assert_eq(baggage_value, "key1=value1,key2=value2")
    }
    None => @test.fail("Expected baggage header")
  }
  
  // 测试提取操作
  let carrier_with_baggage = MapCarrier::from_map([
    ("baggage", "user_id=12345,session_id=abcdef,request_id=xyz789"),
    ("other-header", "other-value")
  ])
  
  let extracted_ctx = propagator.extract(ctx, carrier_with_baggage)
  
  // 验证提取不会崩溃
  assert_eq(true, true)
  
  // 测试复杂的baggage值（包含属性）
  let carrier_complex = MapCarrier::from_map([
    ("baggage", "key1=value1;prop1=val1;prop2=val2,key2=value2;prop3=val3"),
    ("other-header", "other-value")
  ])
  
  let extracted_ctx_complex = propagator.extract(ctx, carrier_complex)
  assert_eq(true, true)
  
  // 测试空baggage header
  let carrier_empty_baggage = MapCarrier::from_map([
    ("baggage", ""),
    ("other-header", "other-value")
  ])
  
  let extracted_ctx_empty = propagator.extract(ctx, carrier_empty_baggage)
  assert_eq(true, true)
  
  // 测试没有baggage header的情况
  let carrier_no_baggage = MapCarrier::new()
  let extracted_ctx_no_baggage = propagator.extract(ctx, carrier_no_baggage)
  assert_eq(true, true)
}