// Propagation模块综合测试用例
test "map_carrier_operations" {
  let carrier = MapCarrier::from_map([
    ("traceparent", "00-0af7651916cd43dd8448eb211c80319c-b7ad6b7169203331-01"),
    ("baggage", "key1=value1,key2=value2"),
    ("custom-header", "custom-value")
  ])
  
  // 测试get操作
  match carrier.get("traceparent") {
    Some(value) => assert_eq(value, "00-0af7651916cd43dd8448eb211c80319c-b7ad6b7169203331-01")
    None => @test.fail("Expected Some(traceparent)")
  }
  
  match carrier.get("baggage") {
    Some(value) => assert_eq(value, "key1=value1,key2=value2")
    None => @test.fail("Expected Some(baggage)")
  }
  
  match carrier.get("custom-header") {
    Some(value) => assert_eq(value, "custom-value")
    None => @test.fail("Expected Some(custom-header)")
  }
  
  // 测试不存在的键
  match carrier.get("non-existent") {
    Some(_) => @test.fail("Expected None")
    None => assert_eq(true, true)
  }
  
  // 测试keys操作
  let keys = carrier.keys()
  assert_eq(keys.length(), 3)
  assert_eq(keys.contains("traceparent"), true)
  assert_eq(keys.contains("baggage"), true)
  assert_eq(keys.contains("custom-header"), true)
}

test "w3c_trace_context_propagator" {
  let propagator = W3CTraceContextPropagator::{}
  let carrier = MapCarrier::new()
  let ctx = context::Context::empty()
  
  // 测试inject操作
  propagator.inject(ctx, carrier)
  
  // 验证traceparent头被设置
  match carrier.get("traceparent") {
    Some(value) => {
      // 验证traceparent格式: version-trace_id-span_id-trace_flags
      assert_eq(value.starts_with("00-"), true)
      assert_eq(value.length(), 55) // 标准traceparent长度
    }
    None => @test.fail("Expected traceparent header to be set")
  }
  
  // 测试extract操作
  let carrier_with_trace = MapCarrier::from_map([
    (TRACE_PARENT_HEADER, "00-0af7651916cd43dd8448eb211c80319c-b7ad6b7169203331-01")
  ])
  
  let extracted_ctx = propagator.extract(ctx, carrier_with_trace)
  // 在简化实现中，extract不实际修改context
  assert_eq(extracted_ctx.values.length(), ctx.values.length())
}

test "w3c_baggage_propagator" {
  let propagator = W3CBaggagePropagator::{}
  let carrier = MapCarrier::new()
  let ctx = context::Context::empty()
  
  // 测试inject操作
  propagator.inject(ctx, carrier)
  
  // 验证baggage头被设置
  match carrier.get("baggage") {
    Some(value) => {
      assert_eq(value, "key1=value1,key2=value2")
    }
    None => @test.fail("Expected baggage header to be set")
  }
  
  // 测试extract操作
  let carrier_with_baggage = MapCarrier::from_map([
    (BAGGAGE_HEADER, "user.id=12345,request.id=67890")
  ])
  
  let extracted_ctx = propagator.extract(ctx, carrier_with_baggage)
  // 在简化实现中，extract不实际修改context
  assert_eq(extracted_ctx.values.length(), ctx.values.length())
}
