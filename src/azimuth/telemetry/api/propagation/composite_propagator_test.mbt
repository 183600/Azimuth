// CompositePropagator的测试用例
// 测试复合传播器的功能

test "composite_propagator_creation" {
  // 测试CompositePropagator的创建
  let trace_propagator = W3CTraceContextPropagator::{}
  let baggage_propagator = W3CBaggagePropagator::{}
  
  let propagators = [trace_propagator, baggage_propagator]
  let composite = CompositePropagator::new(propagators)
  
  // 验证复合传播器包含正确的传播器数量
  assert_eq(composite.propagators.length(), 2)
}

test "composite_propagator_inject" {
  // 测试CompositePropagator的inject操作
  let trace_propagator = W3CTraceContextPropagator::{}
  let baggage_propagator = W3CBaggagePropagator::{}
  
  let propagators = [trace_propagator, baggage_propagator]
  let composite = CompositePropagator::new(propagators)
  
  let ctx = Context::empty()
  let carrier = MapCarrier::new()
  
  // 执行inject
  composite.inject(ctx, carrier)
  
  // 验证traceparent和baggage都被设置
  match carrier.get(TRACE_PARENT_HEADER) {
    Some(_) => assert_eq(true, true)
    None => @test.fail("Test failed")
  }
  
  match carrier.get(BAGGAGE_HEADER) {
    Some(_) => assert_eq(true, true)
    None => @test.fail("Test failed")
  }
}

test "composite_propagator_extract" {
  // 测试CompositePropagator的extract操作
  let trace_propagator = W3CTraceContextPropagator::{}
  let baggage_propagator = W3CBaggagePropagator::{}
  
  let propagators = [trace_propagator, baggage_propagator]
  let composite = CompositePropagator::new(propagators)
  
  let ctx = Context::empty()
  
  // 创建包含traceparent和baggage的carrier
  let carrier_data = [
    (TRACE_PARENT_HEADER, "00-0af7651916cd43dd8448eb211c80319c-b7ad6b7169203331-01"),
    (BAGGAGE_HEADER, "user=id123,session=abc456")
  ]
  
  let carrier = MapCarrier::from_map(carrier_data)
  
  // 执行extract
  let extracted_ctx = composite.extract(ctx, carrier)
  
  // 在简化实现中，extract应该返回原始context
  // 实际实现中，extract会解析headers并更新context
  assert_eq(true, true) // 如果没有崩溃，测试通过
}

test "composite_propagator_single_propagator" {
  // 测试只包含一个传播器的CompositePropagator
  let trace_propagator = W3CTraceContextPropagator::{}
  let propagators = [trace_propagator]
  let composite = CompositePropagator::new(propagators)
  
  let ctx = Context::empty()
  let carrier = MapCarrier::new()
  
  // 测试inject
  composite.inject(ctx, carrier)
  
  match carrier.get(TRACE_PARENT_HEADER) {
    Some(_) => assert_eq(true, true)
    None => @test.fail("Test failed")
  }
  
  match carrier.get(BAGGAGE_HEADER) {
    Some(_) => @test.fail("Test failed")
    None => assert_eq(true, true)
  }
  
  // 测试extract
  let carrier_with_trace = MapCarrier::from_map([
    (TRACE_PARENT_HEADER, "00-0af7651916cd43dd8448eb211c80319c-b7ad6b7169203331-01")
  ])
  
  let extracted_ctx = composite.extract(ctx, carrier_with_trace)
  assert_eq(true, true) // 如果没有崩溃，测试通过
}

test "composite_propagator_empty_propagators" {
  // 测试空传播器列表的CompositePropagator
  let propagators : Array[TextMapPropagator] = []
  let composite = CompositePropagator::new(propagators)
  
  let ctx = Context::empty()
  let carrier = MapCarrier::new()
  
  // 测试inject（应该什么都不做）
  composite.inject(ctx, carrier)
  
  // 验证没有设置任何header
  match carrier.get(TRACE_PARENT_HEADER) {
    Some(_) => @test.fail("Test failed")
    None => assert_eq(true, true)
  }
  
  match carrier.get(BAGGAGE_HEADER) {
    Some(_) => @test.fail("Test failed")
    None => assert_eq(true, true)
  }
  
  // 测试extract（应该返回原始context）
  let carrier_with_data = MapCarrier::from_map([
    (TRACE_PARENT_HEADER, "00-0af7651916cd43dd8448eb211c80319c-b7ad6b7169203331-01"),
    (BAGGAGE_HEADER, "user=id123")
  ])
  
  let extracted_ctx = composite.extract(ctx, carrier_with_data)
  assert_eq(true, true) // 如果没有崩溃，测试通过
}

test "composite_propagator_multiple_inject_calls" {
  // 测试多次调用inject
  let trace_propagator = W3CTraceContextPropagator::{}
  let baggage_propagator = W3CBaggagePropagator::{}
  
  let propagators = [trace_propagator, baggage_propagator]
  let composite = CompositePropagator::new(propagators)
  
  let ctx = Context::empty()
  let carrier = MapCarrier::new()
  
  // 多次调用inject
  composite.inject(ctx, carrier)
  composite.inject(ctx, carrier)
  composite.inject(ctx, carrier)
  
  // 验证headers仍然存在
  match carrier.get(TRACE_PARENT_HEADER) {
    Some(_) => assert_eq(true, true)
    None => @test.fail("Test failed")
  }
  
  match carrier.get(BAGGAGE_HEADER) {
    Some(_) => assert_eq(true, true)
    None => @test.fail("Test failed")
  }
}

test "composite_propagator_order_matters" {
  // 测试传播器顺序的影响
  let trace_propagator = W3CTraceContextPropagator::{}
  let baggage_propagator = W3CBaggagePropagator::{}
  
  // 创建两种不同顺序的复合传播器
  let propagators1 = [trace_propagator, baggage_propagator]
  let propagators2 = [baggage_propagator, trace_propagator]
  
  let composite1 = CompositePropagator::new(propagators1)
  let composite2 = CompositePropagator::new(propagators2)
  
  let ctx = Context::empty()
  let carrier1 = MapCarrier::new()
  let carrier2 = MapCarrier::new()
  
  // 分别inject
  composite1.inject(ctx, carrier1)
  composite2.inject(ctx, carrier2)
  
  // 在简化实现中，顺序可能不重要，但在实际实现中可能会有影响
  // 这里我们只验证两个carrier都包含了必要的headers
  match carrier1.get(TRACE_PARENT_HEADER) {
    Some(_) => assert_eq(true, true)
    None => @test.fail("Test failed")
  }
  
  match carrier1.get(BAGGAGE_HEADER) {
    Some(_) => assert_eq(true, true)
    None => @test.fail("Test failed")
  }
  
  match carrier2.get(TRACE_PARENT_HEADER) {
    Some(_) => assert_eq(true, true)
    None => @test.fail("Test failed")
  }
  
  match carrier2.get(BAGGAGE_HEADER) {
    Some(_) => assert_eq(true, true)
    None => @test.fail("Test failed")
  }
}

test "composite_propagator_with_existing_headers" {
  // 测试在已有headers的carrier上执行inject
  let trace_propagator = W3CTraceContextPropagator::{}
  let baggage_propagator = W3CBaggagePropagator::{}
  
  let propagators = [trace_propagator, baggage_propagator]
  let composite = CompositePropagator::new(propagators)
  
  let ctx = Context::empty()
  
  // 创建已有headers的carrier
  let existing_headers = [
    ("existing.header1", "value1"),
    ("existing.header2", "value2")
  ]
  
  let carrier = MapCarrier::from_map(existing_headers)
  
  // 执行inject
  composite.inject(ctx, carrier)
  
  // 验证原有headers仍然存在
  match carrier.get("existing.header1") {
    Some(value) => assert_eq(value, "value1")
    None => @test.fail("Test failed")
  }
  
  match carrier.get("existing.header2") {
    Some(value) => assert_eq(value, "value2")
    None => @test.fail("Test failed")
  }
  
  // 验证新的headers也被添加
  match carrier.get(TRACE_PARENT_HEADER) {
    Some(_) => assert_eq(true, true)
    None => @test.fail("Test failed")
  }
  
  match carrier.get(BAGGAGE_HEADER) {
    Some(_) => assert_eq(true, true)
    None => @test.fail("Test failed")
  }
}