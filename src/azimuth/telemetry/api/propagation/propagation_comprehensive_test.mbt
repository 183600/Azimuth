// Propagation模块的综合测试用例
test "map_carrier_basic_operations" {
  // 测试MapCarrier的基本操作
  let carrier = MapCarrier::new()
  
  // 验证初始状态
  assert_eq(carrier.data.length(), 0)
  assert_eq(carrier.keys().length(), 0)
  
  // 测试获取不存在的键
  match carrier.get("nonexistent.key") {
    Some(_) => @test.fail("Test failed")
    None => assert_eq(true, true)
  }
  
  // 创建带有初始数据的carrier
  let initial_data = [
    ("traceparent", "00-0af7651916cd43dd8448eb211c80319c-b7ad6b7169203331-01"),
    ("tracestate", "rojo=00f067aa0ba902b7"),
    ("baggage", "key1=value1,key2=value2")
  ]
  
  let carrier_with_data = MapCarrier::from_map(initial_data)
  
  // 验证数据
  assert_eq(carrier_with_data.data.length(), 3)
  assert_eq(carrier_with_data.keys().length(), 3)
  
  // 测试获取存在的键
  match carrier_with_data.get("traceparent") {
    Some(value) => assert_eq(value, "00-0af7651916cd43dd8448eb211c80319c-b7ad6b7169203331-01")
    None => @test.fail("Test failed")
  }
  
  match carrier_with_data.get("tracestate") {
    Some(value) => assert_eq(value, "rojo=00f067aa0ba902b7")
    None => @test.fail("Test failed")
  }
  
  match carrier_with_data.get("baggage") {
    Some(value) => assert_eq(value, "key1=value1,key2=value2")
    None => @test.fail("Test failed")
  }
  
  // 验证keys方法
  let keys = carrier_with_data.keys()
  assert_eq(keys.length(), 3)
  assert_eq(keys.contains("traceparent"), true)
  assert_eq(keys.contains("tracestate"), true)
  assert_eq(keys.contains("baggage"), true)
}

test "w3c_trace_context_propagator_inject" {
  // 测试W3C TraceContext Propagator的inject操作
  let propagator = W3CTraceContextPropagator::{}
  let ctx = Context::empty()
  let carrier = MapCarrier::new()
  
  // 执行inject操作
  propagator.inject(ctx, carrier)
  
  // 验证inject后的carrier（由于是简化实现，我们验证固定值）
  match carrier.get("traceparent") {
    Some(trace_parent) => {
      assert_eq(trace_parent, "00-0af7651916cd43dd8448eb211c80319c-b7ad6b7169203331-01")
      
      // 验证traceparent格式
      assert_eq(trace_parent.has_prefix("00-"), true)
      assert_eq(trace_parent.contains("-"), true)
      
      let parts = trace_parent"-".split_to_string()
      assert_eq(parts.length(), 4)
      assert_eq(parts[0], "00")
      assert_eq(parts[1].length(), 32) // trace_id
      assert_eq(parts[2].length(), 16) // span_id
      assert_eq(parts[3], "01") // trace_flags
    }
    None => @test.fail("Test failed")
  }
}

test "w3c_trace_context_propagator_extract" {
  // 测试W3C TraceContext Propagator的extract操作
  let propagator = W3CTraceContextPropagator::{}
  let ctx = Context::empty()
  
  // 创建包含traceparent的carrier
  let carrier_data = [
    ("traceparent", "00-0af7651916cd43dd8448eb211c80319c-b7ad6b7169203331-01"),
    ("tracestate", "rojo=00f067aa0ba902b7,congo=t61rcWkgMzE")
  ]
  
  let carrier = MapCarrier::from_map(carrier_data)
  
  // 执行extract操作
  let extracted_ctx = propagator.extract(ctx, carrier)
  
  // 在简化实现中，extract不会修改context，但我们验证操作不会出错
  assert_eq(extracted_ctx.values.length(), ctx.values.length())
  
  // 测试没有traceparent的情况
  let empty_carrier = MapCarrier::new()
  let empty_ctx = propagator.extract(Context::empty(), empty_carrier)
  assert_eq(empty_ctx.values.length(), 0)
}

test "w3c_baggage_propagator_inject" {
  // 测试W3C Baggage Propagator的inject操作
  let propagator = W3CBaggagePropagator::{}
  let ctx = Context::empty()
  let carrier = MapCarrier::new()
  
  // 执行inject操作
  propagator.inject(ctx, carrier)
  
  // 验证inject后的carrier（由于是简化实现，我们验证固定值）
  match carrier.get("baggage") {
    Some(baggage_value) => {
      assert_eq(baggage_value, "key1=value1,key2=value2")
      
      // 验证baggage格式
      assert_eq(baggage_value.contains("="), true)
      assert_eq(baggage_value.contains(","), true)
      
      let entries = baggage_value",".split_to_string()
      assert_eq(entries.length(), 2)
      
      let entry1_parts = entries[0]"=".split_to_string()
      assert_eq(entry1_parts[0], "key1")
      assert_eq(entry1_parts[1], "value1")
      
      let entry2_parts = entries[1]"=".split_to_string()
      assert_eq(entry2_parts[0], "key2")
      assert_eq(entry2_parts[1], "value2")
    }
    None => @test.fail("Test failed")
  }
}

test "w3c_baggage_propagator_extract" {
  // 测试W3C Baggage Propagator的extract操作
  let propagator = W3CBaggagePropagator::{}
  let ctx = Context::empty()
  
  // 创建包含baggage的carrier
  let carrier_data = [
    ("baggage", "user.id=12345,request.id=67890,service.name=api-gateway"),
    ("other-header", "other-value")
  ]
  
  let carrier = MapCarrier::from_map(carrier_data)
  
  // 执行extract操作
  let extracted_ctx = propagator.extract(ctx, carrier)
  
  // 在简化实现中，extract不会修改context，但我们验证操作不会出错
  assert_eq(extracted_ctx.values.length(), ctx.values.length())
  
  // 测试没有baggage的情况
  let empty_carrier = MapCarrier::new()
  let empty_ctx = propagator.extract(Context::empty(), empty_carrier)
  assert_eq(empty_ctx.values.length(), 0)
}

test "composite_propagator_creation_and_usage" {
  // 测试Composite Propagator的创建和使用
  let trace_propagator = W3CTraceContextPropagator::{}
  let baggage_propagator = W3CBaggagePropagator::{}
  
  // 创建composite propagator
  let propagators = [trace_propagator, baggage_propagator]
  let composite_propagator = CompositePropagator::new(propagators)
  
  // 验证composite propagator
  assert_eq(composite_propagator.propagators.length(), 2)
  
  // 测试inject操作
  let ctx = Context::empty()
  let carrier = MapCarrier::new()
  
  composite_propagator.inject(ctx, carrier)
  
  // 验证两个propagator都执行了inject
  match carrier.get("traceparent") {
    Some(_) => assert_eq(true, true)
    None => @test.fail("Test failed")
  }
  
  match carrier.get("baggage") {
    Some(_) => assert_eq(true, true)
    None => @test.fail("Test failed")
  }
  
  // 测试extract操作
  let carrier_with_data = MapCarrier::from_map([
    ("traceparent", "00-0af7651916cd43dd8448eb211c80319c-b7ad6b7169203331-01"),
    ("baggage", "key1=value1,key2=value2")
  ])
  
  let extracted_ctx = composite_propagator.extract(Context::empty(), carrier_with_data)
  assert_eq(extracted_ctx.values.length(), 0) // 简化实现中不会修改context
}

test "composite_propagator_multiple_propagators" {
  // 测试Composite Propagator包含多个propagators
  let trace_propagator = W3CTraceContextPropagator::{}
  let baggage_propagator = W3CBaggagePropagator::{}
  
  // 创建包含重复propagators的composite
  let propagators = [
    trace_propagator,
    baggage_propagator,
    trace_propagator, // 重复的trace propagator
    baggage_propagator  // 重复的baggage propagator
  ]
  let composite_propagator = CompositePropagator::new(propagators)
  
  assert_eq(composite_propagator.propagators.length(), 4)
  
  // 测试inject操作（应该执行4次）
  let ctx = Context::empty()
  let carrier = MapCarrier::new()
  
  composite_propagator.inject(ctx, carrier)
  
  // 验证header存在（由于是简化实现，多次inject会覆盖相同的值）
  match carrier.get("traceparent") {
    Some(_) => assert_eq(true, true)
    None => @test.fail("Test failed")
  }
  
  match carrier.get("baggage") {
    Some(_) => assert_eq(true, true)
    None => @test.fail("Test failed")
  }
}

test "propagation_edge_cases" {
  // 测试传播的边界情况
  
  // 空的Composite Propagator
  let empty_propagators = []
  let empty_composite = CompositePropagator::new(empty_propagators)
  assert_eq(empty_composite.propagators.length(), 0)
  
  // 空propagator的inject和extract不应该出错
  let ctx = Context::empty()
  let carrier = MapCarrier::new()
  
  empty_composite.inject(ctx, carrier)
  let extracted_ctx = empty_composite.extract(ctx, carrier)
  
  // 验证carrier没有被修改
  match carrier.get("traceparent") {
    Some(_) => @test.fail("Test failed")
    None => assert_eq(true, true)
  }
  
  // 特殊字符的header值
  let special_carrier = MapCarrier::from_map([
    ("traceparent", "00-0af7651916cd43dd8448eb211c80319c-b7ad6b7169203331-01"),
    ("baggage", "key.with.dots=value.with.special.chars!@#$%^&*()"),
    ("unicode.header", "Unicode值测试🚀")
  ])
  
  let trace_propagator = W3CTraceContextPropagator::{}
  let baggage_propagator = W3CBaggagePropagator::{}
  let composite = CompositePropagator::new([trace_propagator, baggage_propagator])
  
  // extract操作应该能处理特殊字符
  let special_ctx = composite.extract(Context::empty(), special_carrier)
  assert_eq(special_ctx.values.length(), 0)
}

test "propagation_with_complex_context" {
  // 测试与复杂context的传播
  
  // 创建包含多个值的context
  let ctx = Context::empty()
  let key1 = create_key("custom.key1")
  let key2 = create_key("custom.key2")
  let key3 = create_key("custom.key3")
  
  let complex_ctx = ctx
    .with_value(key1, "value1")
    .with_value(key2, "value2")
    .with_value(key3, "value3")
  
  assert_eq(complex_ctx.values.length(), 3)
  
  // 测试传播不会影响现有的context值
  let trace_propagator = W3CTraceContextPropagator::{}
  let baggage_propagator = W3CBaggagePropagator::{}
  let composite = CompositePropagator::new([trace_propagator, baggage_propagator])
  
  let carrier = MapCarrier::new()
  composite.inject(complex_ctx, carrier)
  
  // 验证context的值没有被修改
  match complex_ctx.get(key1) {
    Some(value) => assert_eq(value, "value1")
    None => @test.fail("Test failed")
  }
  
  match complex_ctx.get(key2) {
    Some(value) => assert_eq(value, "value2")
    None => @test.fail("Test failed")
  }
  
  match complex_ctx.get(key3) {
    Some(value) => assert_eq(value, "value3")
    None => @test.fail("Test failed")
  }
  
  // 测试extract不会覆盖现有的context值
  let carrier_with_data = MapCarrier::from_map([
    ("traceparent", "00-0af7651916cd43dd8448eb211c80319c-b7ad6b7169203331-01"),
    ("baggage", "extracted.key=extracted.value")
  ])
  
  let extracted_ctx = composite.extract(complex_ctx, carrier_with_data)
  
  // 在简化实现中，extract不会修改context
  assert_eq(extracted_ctx.values.length(), 3)
  match extracted_ctx.get(key1) {
    Some(value) => assert_eq(value, "value1")
    None => @test.fail("Test failed")
  }
}

test "map_carrier_to_map_conversion" {
  // 测试MapCarrier的to_map转换
  let original_data = [
    ("key1", "value1"),
    ("key2", "value2"),
    ("key3", "value3")
  ]
  
  let carrier = MapCarrier::from_map(original_data)
  let converted_data = carrier.to_map()
  
  // 验证转换后的数据
  assert_eq(converted_data.length(), 3)
  
  let mut i = 0
  while i < original_data.length() {
    let (original_key, original_value) = original_data[i]
    let (converted_key, converted_value) = converted_data[i]
    
    assert_eq(original_key, converted_key)
    assert_eq(original_value, converted_value)
    i = i + 1
  }
  
  // 验证数据内容
  match carrier.get("key1") {
    Some(value) => assert_eq(value, "value1")
    None => @test.fail("Test failed")
  }
  
  match carrier.get("key2") {
    Some(value) => assert_eq(value, "value2")
    None => @test.fail("Test failed")
  }
  
  match carrier.get("key3") {
    Some(value) => assert_eq(value, "value3")
    None => @test.fail("Test failed")
  }
}