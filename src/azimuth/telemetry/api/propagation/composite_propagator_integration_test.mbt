// 复合传播器集成测试
test "w3c_trace_context_propagator_basic" {
  // 测试W3C TraceContext Propagator的基本功能
  
  let ctx = Context::empty()
  let carrier = MapCarrier::new()
  let propagator = W3CTraceContextPropagator::{}
  
  // 测试注入
  propagator.inject(ctx, carrier)
  
  // 验证注入的头部
  match carrier.get(TRACE_PARENT_HEADER) {
    Some(trace_parent) => {
      assert_eq(trace_parent.has_prefix("00-"), true)
      assert_eq(trace_parent.length(), 55) // "00-" + 32 chars + "-" + 16 chars + "-01"
      assert_eq(trace_parent.has_suffix("-01"), true)
    }
    None => @test.fail("Test failed")
  }
  
  // 测试提取
  let extracted_ctx = propagator.extract(ctx, carrier)
  assert_eq(true, true) // 验证提取操作不会出错
}

test "w3c_baggage_propagator_basic" {
  // 测试W3C Baggage Propagator的基本功能
  
  let ctx = Context::empty()
  let carrier = MapCarrier::new()
  let propagator = W3CBaggagePropagator::{}
  
  // 测试注入
  propagator.inject(ctx, carrier)
  
  // 验证注入的头部
  match carrier.get(BAGGAGE_HEADER) {
    Some(baggage_value) => {
      assert_eq(baggage_value.contains("key1=value1"), true)
      assert_eq(baggage_value.contains("key2=value2"), true)
      assert_eq(baggage_value.contains(","), true)
    }
    None => @test.fail("Test failed")
  }
  
  // 测试提取
  let extracted_ctx = propagator.extract(ctx, carrier)
  assert_eq(true, true) // 验证提取操作不会出错
}

test "composite_propagator_single" {
  // 测试包含单个传播器的复合传播器
  
  let ctx = Context::empty()
  let carrier = MapCarrier::new()
  
  // 创建只包含W3C TraceContext的复合传播器
  let trace_propagator = W3CTraceContextPropagator::{}
  let propagators = [trace_propagator]
  let composite = CompositePropagator::new(propagators)
  
  // 测试注入
  composite.inject(ctx, carrier)
  
  // 验证只有traceparent头部被设置
  match carrier.get(TRACE_PARENT_HEADER) {
    Some(_) => assert_eq(true, true)
    None => @test.fail("Test failed")
  }
  
  match carrier.get(BAGGAGE_HEADER) {
    Some(_) => @test.fail("Test failed")
    None => assert_eq(true, true)
  }
  
  // 测试提取
  let extracted_ctx = composite.extract(ctx, carrier)
  assert_eq(true, true)
}

test "composite_propagator_multiple" {
  // 测试包含多个传播器的复合传播器
  
  let ctx = Context::empty()
  let carrier = MapCarrier::new()
  
  // 创建包含两个传播器的复合传播器
  let trace_propagator = W3CTraceContextPropagator::{}
  let baggage_propagator = W3CBaggagePropagator::{}
  let propagators = [trace_propagator, baggage_propagator]
  let composite = CompositePropagator::new(propagators)
  
  // 测试注入
  composite.inject(ctx, carrier)
  
  // 验证两个头部都被设置
  match carrier.get(TRACE_PARENT_HEADER) {
    Some(trace_parent) => {
      assert_eq(trace_parent.has_prefix("00-"), true)
      assert_eq(trace_parent.length(), 55)
    }
    None => @test.fail("Test failed")
  }
  
  match carrier.get(BAGGAGE_HEADER) {
    Some(baggage_value) => {
      assert_eq(baggage_value.contains("key1=value1"), true)
      assert_eq(baggage_value.contains("key2=value2"), true)
    }
    None => @test.fail("Test failed")
  }
  
  // 测试提取
  let extracted_ctx = composite.extract(ctx, carrier)
  assert_eq(true, true)
}

test "composite_propagator_order_independence" {
  // 测试复合传播器中传播器顺序的独立性
  
  let ctx = Context::empty()
  let carrier1 = MapCarrier::new()
  let carrier2 = MapCarrier::new()
  
  let trace_propagator = W3CTraceContextPropagator::{}
  let baggage_propagator = W3CBaggagePropagator::{}
  
  // 创建不同顺序的复合传播器
  let composite1 = CompositePropagator::new([trace_propagator, baggage_propagator])
  let composite2 = CompositePropagator::new([baggage_propagator, trace_propagator])
  
  // 测试注入
  composite1.inject(ctx, carrier1)
  composite2.inject(ctx, carrier2)
  
  // 验证两个carrier都包含相同的头部
  match carrier1.get(TRACE_PARENT_HEADER) {
    Some(trace_parent1) => {
      match carrier2.get(TRACE_PARENT_HEADER) {
        Some(trace_parent2) => assert_eq(trace_parent1, trace_parent2)
        None => @test.fail("Test failed")
      }
    }
    None => @test.fail("Test failed")
  }
  
  match carrier1.get(BAGGAGE_HEADER) {
    Some(baggage1) => {
      match carrier2.get(BAGGAGE_HEADER) {
        Some(baggage2) => assert_eq(baggage1, baggage2)
        None => @test.fail("Test failed")
      }
    }
    None => @test.fail("Test failed")
  }
}

test "map_carrier_operations" {
  // 测试MapCarrier的基本操作
  
  let initial_data = [
    ("existing-key", "existing-value"),
    ("another-key", "another-value")
  ]
  let carrier = MapCarrier::from_map(initial_data)
  
  // 测试获取现有键
  match carrier.get("existing-key") {
    Some(value) => assert_eq(value, "existing-value")
    None => @test.fail("Test failed")
  }
  
  match carrier.get("another-key") {
    Some(value) => assert_eq(value, "another-value")
    None => @test.fail("Test failed")
  }
  
  // 测试获取不存在的键
  match carrier.get("non-existent") {
    Some(_) => @test.fail("Test failed")
    None => assert_eq(true, true)
  }
  
  // 测试获取所有键
  let keys = carrier.keys()
  assert_eq(keys.length(), 2)
  assert_eq(keys.contains("existing-key"), true)
  assert_eq(keys.contains("another-key"), true)
  
  // 测试转换为map
  let map_data = carrier.to_map()
  assert_eq(map_data.length(), 2)
  assert_eq(map_data[0].0, "existing-key")
  assert_eq(map_data[0].1, "existing-value")
  assert_eq(map_data[1].0, "another-key")
  assert_eq(map_data[1].1, "another-value")
}

test "composite_propagator_with_empty_carrier" {
  // 测试复合传播器与空carrier的交互
  
  let ctx = Context::empty()
  let empty_carrier = MapCarrier::new()
  
  let trace_propagator = W3CTraceContextPropagator::{}
  let baggage_propagator = W3CBaggagePropagator::{}
  let composite = CompositePropagator::new([trace_propagator, baggage_propagator])
  
  // 测试从空carrier提取
  let extracted_ctx = composite.extract(ctx, empty_carrier)
  assert_eq(true, true) // 验证不会出错
  
  // 测试向空carrier注入
  composite.inject(ctx, empty_carrier)
  
  // 验证注入后的carrier包含头部
  match empty_carrier.get(TRACE_PARENT_HEADER) {
    Some(_) => assert_eq(true, true)
    None => @test.fail("Test failed")
  }
  
  match empty_carrier.get(BAGGAGE_HEADER) {
    Some(_) => assert_eq(true, true)
    None => @test.fail("Test failed")
  }
}

test "composite_propagator_with_preexisting_data" {
  // 测试复合传播器与预存在数据的carrier的交互
  
  let ctx = Context::empty()
  let preexisting_data = [
    ("custom-header", "custom-value"),
    ("another-header", "another-value"),
    (TRACE_PARENT_HEADER, "existing-traceparent"),
    (BAGGAGE_HEADER, "existing-baggage")
  ]
  let carrier = MapCarrier::from_map(preexisting_data)
  
  let trace_propagator = W3CTraceContextPropagator::{}
  let baggage_propagator = W3CBaggagePropagator::{}
  let composite = CompositePropagator::new([trace_propagator, baggage_propagator])
  
  // 测试从包含预存数据的carrier提取
  let extracted_ctx = composite.extract(ctx, carrier)
  assert_eq(true, true) // 验证不会出错
  
  // 验证预存数据仍然存在
  match carrier.get("custom-header") {
    Some(value) => assert_eq(value, "custom-value")
    None => @test.fail("Test failed")
  }
  
  match carrier.get("another-header") {
    Some(value) => assert_eq(value, "another-value")
    None => @test.fail("Test failed")
  }
  
  // 测试注入（可能会覆盖现有的头部）
  composite.inject(ctx, carrier)
  
  // 验证所有键仍然存在
  let keys = carrier.keys()
  assert_eq(keys.length(), 4)
  assert_eq(keys.contains("custom-header"), true)
  assert_eq(keys.contains("another-header"), true)
  assert_eq(keys.contains(TRACE_PARENT_HEADER), true)
  assert_eq(keys.contains(BAGGAGE_HEADER), true)
}

test "composite_propagator_large_scale" {
  // 测试复合传播器的大规模操作
  
  let ctx = Context::empty()
  let carrier = MapCarrier::new()
  
  // 创建包含多个传播器的复合传播器
  let mut propagators = []
  let mut i = 0
  while i < 10 {
    propagators.push(W3CTraceContextPropagator::{})
    propagators.push(W3CBaggagePropagator::{})
    i = i + 1
  }
  
  let composite = CompositePropagator::new(propagators)
  
  // 测试注入
  composite.inject(ctx, carrier)
  
  // 验证carrier包含预期的头部
  match carrier.get(TRACE_PARENT_HEADER) {
    Some(trace_parent) => {
      assert_eq(trace_parent.has_prefix("00-"), true)
      assert_eq(trace_parent.length(), 55)
    }
    None => @test.fail("Test failed")
  }
  
  match carrier.get(BAGGAGE_HEADER) {
    Some(baggage_value) => {
      assert_eq(baggage_value.contains("key1=value1"), true)
      assert_eq(baggage_value.contains("key2=value2"), true)
    }
    None => @test.fail("Test failed")
  }
  
  // 测试提取
  let extracted_ctx = composite.extract(ctx, carrier)
  assert_eq(true, true) // 验证不会出错
  
  // 验证carrier的键
  let keys = carrier.keys()
  assert_eq(keys.length(), 2)
  assert_eq(keys.contains(TRACE_PARENT_HEADER), true)
  assert_eq(keys.contains(BAGGAGE_HEADER), true)
}

test "propagator_error_handling" {
  // 测试传播器的错误处理
  
  let ctx = Context::empty()
  
  // 测试空数据carrier
  let empty_carrier = MapCarrier::from_map([])
  let trace_propagator = W3CTraceContextPropagator::{}
  let baggage_propagator = W3CBaggagePropagator::{}
  
  // 测试从空carrier提取
  let extracted_ctx1 = trace_propagator.extract(ctx, empty_carrier)
  let extracted_ctx2 = baggage_propagator.extract(ctx, empty_carrier)
  assert_eq(true, true) // 验证不会出错
  
  // 测试向空carrier注入
  trace_propagator.inject(ctx, empty_carrier)
  baggage_propagator.inject(ctx, empty_carrier)
  
  // 验证注入后的数据
  match empty_carrier.get(TRACE_PARENT_HEADER) {
    Some(_) => assert_eq(true, true)
    None => @test.fail("Test failed")
  }
  
  match empty_carrier.get(BAGGAGE_HEADER) {
    Some(_) => assert_eq(true, true)
    None => @test.fail("Test failed")
  }
}