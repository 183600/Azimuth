// Propagation模块传播器测试
// 测试上下文传播器的注入和提取功能

test "map_carrier_basic_operations" {
  let carrier = MapCarrier::new()
  
  // 测试初始状态
  assert_eq(carrier.data.length(), 0)
  assert_eq(carrier.keys().length(), 0)
  
  // 测试从map创建carrier
  let initial_data = [
    ("key1", "value1"),
    ("key2", "value2")
  ]
  let populated_carrier = MapCarrier::from_map(initial_data)
  assert_eq(populated_carrier.data.length(), 2)
  
  // 测试获取值
  match populated_carrier.get("key1") {
    Some(value) => assert_eq(value, "value1")
    None => @test.fail("Expected value for key1")
  }
  
  match populated_carrier.get("nonexistent") {
    Some(_) => @test.fail("Expected None for nonexistent key")
    None => @test.succeed()
  }
  
  // 测试获取键列表
  let keys = populated_carrier.keys()
  assert_eq(keys.length(), 2)
  assert_eq(keys[0], "key1")
  assert_eq(keys[1], "key2")
}

test "w3c_trace_context_propagator_inject" {
  let ctx = Context::empty()
  let carrier = MapCarrier::new()
  let propagator = W3CTraceContextPropagator::{}
  
  propagator.inject(ctx, carrier)
  
  // 验证是否注入了traceparent头
  match carrier.get("traceparent") {
    Some(trace_parent) => {
      // 验证traceparent格式：version-trace_id-span_id-trace_flags
      assert_eq(trace_parent.length(), 55)  // 标准W3C traceparent长度
      assert_eq(trace_parent[0..2], "00")   // 版本号
    }
    None => @test.fail("Expected traceparent header")
  }
}

test "w3c_trace_context_propagator_extract" {
  let ctx = Context::empty()
  let trace_data = [
    ("traceparent", "00-0af7651916cd43dd8448eb211c80319c-b7ad6b7169203331-01"),
    ("other-header", "other-value")
  ]
  let carrier = MapCarrier::from_map(trace_data)
  let propagator = W3CTraceContextPropagator::{}
  
  let extracted_ctx = propagator.extract(ctx, carrier)
  
  // 在简化实现中，extract不会修改context
  // 但在实际实现中应该解析traceparent并设置相应的上下文值
  assert_eq(extracted_ctx.values.length(), 0)  // 当前Noop实现
}

test "w3c_baggage_propagator_inject" {
  let ctx = Context::empty()
  let carrier = MapCarrier::new()
  let propagator = W3CBaggagePropagator::{}
  
  propagator.inject(ctx, carrier)
  
  // 验证是否注入了baggage头
  match carrier.get("baggage") {
    Some(baggage_value) => {
      assert_eq(baggage_value, "key1=value1,key2=value2")
    }
    None => @test.fail("Expected baggage header")
  }
}

test "w3c_baggage_propagator_extract" {
  let ctx = Context::empty()
  let baggage_data = [
    ("baggage", "key1=value1,key2=value2"),
    ("other-header", "other-value")
  ]
  let carrier = MapCarrier::from_map(baggage_data)
  let propagator = W3CBaggagePropagator::{}
  
  let extracted_ctx = propagator.extract(ctx, carrier)
  
  // 在简化实现中，extract不会修改context
  // 但在实际实现中应该解析baggage头并更新Baggage
  assert_eq(extracted_ctx.values.length(), 0)  // 当前Noop实现
}

test "composite_propagator_creation" {
  let trace_propagator = W3CTraceContextPropagator::{}
  let baggage_propagator = W3CBaggagePropagator::{}
  let propagators = [trace_propagator, baggage_propagator]
  
  let composite = CompositePropagator::new(propagators)
  assert_eq(composite.propagators.length(), 2)
}

test "composite_propagator_inject" {
  let ctx = Context::empty()
  let carrier = MapCarrier::new()
  let trace_propagator = W3CTraceContextPropagator::{}
  let baggage_propagator = W3CBaggagePropagator::{}
  let propagators = [trace_propagator, baggage_propagator]
  
  let composite = CompositePropagator::new(propagators)
  composite.inject(ctx, carrier)
  
  // 验证两个传播器都执行了注入
  match carrier.get("traceparent") {
    Some(_) => @test.succeed()
    None => @test.fail("Expected traceparent header")
  }
  
  match carrier.get("baggage") {
    Some(_) => @test.succeed()
    None => @test.fail("Expected baggage header")
  }
}

test "composite_propagator_extract" {
  let ctx = Context::empty()
  let headers_data = [
    ("traceparent", "00-0af7651916cd43dd8448eb211c80319c-b7ad6b7169203331-01"),
    ("baggage", "key1=value1,key2=value2")
  ]
  let carrier = MapCarrier::from_map(headers_data)
  let trace_propagator = W3CTraceContextPropagator::{}
  let baggage_propagator = W3CBaggagePropagator::{}
  let propagators = [trace_propagator, baggage_propagator]
  
  let composite = CompositePropagator::new(propagators)
  let extracted_ctx = composite.extract(ctx, carrier)
  
  // 在简化实现中，extract不会修改context
  assert_eq(extracted_ctx.values.length(), 0)
}

test "propagator_with_empty_carrier" {
  let ctx = Context::empty()
  let empty_carrier = MapCarrier::new()
  let trace_propagator = W3CTraceContextPropagator::{}
  let baggage_propagator = W3CBaggagePropagator::{}
  
  // 测试从空carrier提取
  let extracted_ctx1 = trace_propagator.extract(ctx, empty_carrier)
  let extracted_ctx2 = baggage_propagator.extract(ctx, empty_carrier)
  
  assert_eq(extracted_ctx1.values.length(), 0)
  assert_eq(extracted_ctx2.values.length(), 0)
  
  // 测试向空carrier注入
  trace_propagator.inject(ctx, empty_carrier)
  baggage_propagator.inject(ctx, empty_carrier)
  
  // 验证注入后的状态
  match empty_carrier.get("traceparent") {
    Some(_) => @test.succeed()
    None => @test.fail("Expected traceparent header after inject")
  }
  
  match empty_carrier.get("baggage") {
    Some(_) => @test.succeed()
    None => @test.fail("Expected baggage header after inject")
  }
}

test "propagator_multiple_operations" {
  let ctx = Context::empty()
  let carrier = MapCarrier::new()
  let composite = CompositePropagator::new([
    W3CTraceContextPropagator::{},
    W3CBaggagePropagator::{}
  ])
  
  // 执行多次注入和提取操作
  composite.inject(ctx, carrier)
  let extracted_ctx1 = composite.extract(ctx, carrier)
  
  composite.inject(extracted_ctx1, carrier)
  let extracted_ctx2 = composite.extract(extracted_ctx1, carrier)
  
  // 验证操作不会出错
  assert_eq(carrier.keys().length(), 2)
  assert_eq(extracted_ctx1.values.length(), 0)
  assert_eq(extracted_ctx2.values.length(), 0)
}