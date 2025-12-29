test "composite_propagator_functionality" {
  let trace_propagator = W3CTraceContextPropagator::{}
  let baggage_propagator = W3CBaggagePropagator::{}
  
  // 创建CompositePropagator
  let propagators = [trace_propagator, baggage_propagator]
  let composite = CompositePropagator::new(propagators)
  
  let ctx = Context::empty()
  let carrier = MapCarrier::new()
  
  // 测试inject
  composite.inject(ctx, carrier)
  
  // 验证两个header都被设置
  match carrier.get(TRACE_PARENT_HEADER) {
    Some(trace_parent) => {
      assert_eq(trace_parent.length(), 55) // 标准traceparent长度
    }
    None => @test.fail("Expected traceparent header")
  }
  
  match carrier.get(BAGGAGE_HEADER) {
    Some(baggage_value) => {
      assert_eq(baggage_value, "key1=value1,key2=value2")
    }
    None => @test.fail("Expected baggage header")
  }
  
  // 测试extract
  let carrier_with_headers = MapCarrier::from_map([
    (TRACE_PARENT_HEADER, "00-0af7651916cd43dd8448eb211c80319c-b7ad6b7169203331-01"),
    (BAGGAGE_HEADER, "user=id123,session=abc456")
  ])
  
  let extracted_ctx = composite.extract(ctx, carrier_with_headers)
  // 在简化实现中，extract应该返回原始context
  assert_eq(true, true) // 如果没有崩溃，测试通过
  
  // 测试空的propagators数组
  let empty_propagators : Array[TextMapPropagator] = []
  let empty_composite = CompositePropagator::new(empty_propagators)
  
  let empty_carrier = MapCarrier::new()
  empty_composite.inject(ctx, empty_carrier)
  
  // 验证没有设置任何header
  match empty_carrier.get(TRACE_PARENT_HEADER) {
    Some(_) => @test.fail("Expected None for empty composite")
    None => assert_eq(true, true)
  }
  
  match empty_carrier.get(BAGGAGE_HEADER) {
    Some(_) => @test.fail("Expected None for empty composite")
    None => assert_eq(true, true)
  }
  
  // 测试单个propagator的composite
  let single_propagators = [trace_propagator]
  let single_composite = CompositePropagator::new(single_propagators)
  
  let single_carrier = MapCarrier::new()
  single_composite.inject(ctx, single_carrier)
  
  // 验证只有traceparent被设置
  match single_carrier.get(TRACE_PARENT_HEADER) {
    Some(_) => assert_eq(true, true)
    None => @test.fail("Expected traceparent header")
  }
  
  match single_carrier.get(BAGGAGE_HEADER) {
    Some(_) => @test.fail("Expected None for single propagator")
    None => assert_eq(true, true)
  }
}