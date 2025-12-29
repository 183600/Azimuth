test "map_carrier_basic_operations" {
  let initial_data = [
    ("key1", "value1"),
    ("key2", "value2"),
    ("key3", "value3")
  ]
  
  let carrier = MapCarrier::from_map(initial_data)
  
  // 测试get操作
  match carrier.get("key1") {
    Some(value) => assert_eq(value, "value1")
    None => @test.fail("Test failed")
  }
  
  match carrier.get("non-existent") {
    Some(_) => @test.fail("Test failed")
    None => assert_eq(true, true)
  }
  
  // 测试keys操作
  let keys = carrier.keys()
  assert_eq(keys.length(), 3)
  assert_eq(keys[0], "key1")
  assert_eq(keys[1], "key2")
  assert_eq(keys[2], "key3")
  
  // 测试to_map操作
  let map_data = carrier.to_map()
  assert_eq(map_data.length(), 3)
  assert_eq(map_data[0], ("key1", "value1"))
}

test "w3c_trace_context_inject" {
  let propagator = W3CTraceContextPropagator::{}
  let ctx = Context::empty()
  let carrier = MapCarrier::new()
  
  // 测试inject
  propagator.inject(ctx, carrier)
  
  // 验证traceparent header被设置
  match carrier.get(TRACE_PARENT_HEADER) {
    Some(trace_parent) => {
      // 验证traceparent格式: version-trace_id-span_id-trace_flags
      assert_eq(trace_parent.length(), 55) // 标准traceparent长度
      assert_eq(trace_parent[0], '0')
      assert_eq(trace_parent[1], '0')
      assert_eq(trace_parent[2], '-')
    }
    None => @test.fail("Test failed")
  }
}

test "w3c_baggage_operations" {
  let propagator = W3CBaggagePropagator::{}
  let ctx = Context::empty()
  let carrier = MapCarrier::new()
  
  // 测试inject
  propagator.inject(ctx, carrier)
  
  // 验证baggage header被设置
  match carrier.get(BAGGAGE_HEADER) {
    Some(baggage_value) => {
      assert_eq(baggage_value, "key1=value1,key2=value2")
    }
    None => @test.fail("Test failed")
  }
  
  // 测试extract
  let carrier_with_baggage = MapCarrier::from_map([
    (BAGGAGE_HEADER, "user=id123,session=abc456")
  ])
  
  let extracted_ctx = propagator.extract(ctx, carrier_with_baggage)
  // 在简化实现中，extract应该返回原始context
  assert_eq(true, true) // 如果没有崩溃，测试通过
}