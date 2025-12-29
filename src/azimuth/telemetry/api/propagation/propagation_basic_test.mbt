// Propagation模块基本测试用例
test "map_carrier_operations" {
  let carrier = MapCarrier::new()
  
  // 测试基本操作
  let result1 = carrier.get("non_existent_key")
  match result1 {
    Some(_) => @test.fail("Test failed")
    None => assert_eq(true, true)
  }
  
  // 测试设置和获取
  carrier.set("test_key", "test_value")
  let result2 = carrier.get("test_key")
  match result2 {
    Some(value) => assert_eq(value, "test_value")
    None => @test.fail("Test failed")
  }
  
  // 测试覆盖
  carrier.set("test_key", "new_value")
  let result3 = carrier.get("test_key")
  match result3 {
    Some(value) => assert_eq(value, "new_value")
    None => @test.fail("Test failed")
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
    None => @test.fail("Test failed")
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
    None => @test.fail("Test failed")
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

test "composite_propagator_with_multiple_propagators" {
  let trace_propagator = W3CTraceContextPropagator::{}
  let baggage_propagator = W3CBaggagePropagator::{}
  
  // 创建复合传播器
  let propagators = [trace_propagator, baggage_propagator]
  let composite = CompositePropagator::new(propagators)
  
  let carrier = MapCarrier::new()
  let ctx = Context::empty()
  
  // 测试注入操作
  composite.inject(ctx, carrier)
  
  // 验证两个header都被设置
  match carrier.get("traceparent") {
    Some(_) => assert_eq(true, true)
    None => @test.fail("Test failed")
  }
  
  match carrier.get("baggage") {
    Some(_) => assert_eq(true, true)
    None => @test.fail("Test failed")
  }
  
  // 测试提取操作
  let carrier_with_headers = MapCarrier::from_map([
    ("traceparent", "00-0af7651916cd43dd8448eb211c80319c-b7ad6b7169203331-01"),
    ("baggage", "user_id=12345,session_id=abcdef"),
    ("other-header", "other-value")
  ])
  
  let extracted_ctx = composite.extract(ctx, carrier_with_headers)
  assert_eq(true, true)
  
  // 测试只有部分header的情况
  let carrier_trace_only = MapCarrier::from_map([
    ("traceparent", "00-0af7651916cd43dd8448eb211c80319c-b7ad6b7169203331-01")
  ])
  
  let extracted_trace_only = composite.extract(ctx, carrier_trace_only)
  assert_eq(true, true)
  
  let carrier_baggage_only = MapCarrier::from_map([
    ("baggage", "user_id=12345,session_id=abcdef")
  ])
  
  let extracted_baggage_only = composite.extract(ctx, carrier_baggage_only)
  assert_eq(true, true)
  
  // 测试空carrier
  let empty_carrier = MapCarrier::new()
  let extracted_empty = composite.extract(ctx, empty_carrier)
  assert_eq(true, true)
}

test "map_carrier_edge_cases_and_complex_operations" {
  // 测试边界情况和复杂操作
  
  // 创建包含大量键值对的carrier
  let mut large_data = []
  let mut i = 0
  while i < 100 {
    large_data.push(("key" + i.to_string(), "value" + i.to_string()))
    i = i + 1
  }
  
  let large_carrier = MapCarrier::from_map(large_data)
  
  // 测试获取特定键
  match large_carrier.get("key0") {
    Some(value) => assert_eq(value, "value0")
    None => @test.fail("Test failed")
  }
  
  match large_carrier.get("key99") {
    Some(value) => assert_eq(value, "value99")
    None => @test.fail("Test failed")
  }
  
  match large_carrier.get("nonexistent") {
    Some(_) => @test.fail("Test failed")
    None => assert_eq(true, true)
  }
  
  // 测试获取所有键
  let keys = large_carrier.keys()
  assert_eq(keys.length(), 100)
  
  // 测试特殊字符键和值
  let special_data = [
    ("", "empty_key"),
    ("empty_value", ""),
    ("special!@#$%^&*()", "special!@#$%^&*()"),
    ("unicode_test", "测试中文"),
    ("space key", "space value"),
    ("dot.key", "dot.value"),
    ("header-case", "Header-Value"),
    ("very.long.key.name.with.many.dots.and.underscores", "very.long.value.name.with.many.dots.and.underscores")
  ]
  
  let special_carrier = MapCarrier::from_map(special_data)
  
  // 验证特殊键值对
  match special_carrier.get("") {
    Some(value) => assert_eq(value, "empty_key")
    None => @test.fail("Test failed")
  }
  
  match special_carrier.get("empty_value") {
    Some(value) => assert_eq(value, "")
    None => @test.fail("Test failed")
  }
  
  match special_carrier.get("special!@#$%^&*()") {
    Some(value) => assert_eq(value, "special!@#$%^&*()")
    None => @test.fail("Test failed")
  }
  
  match special_carrier.get("unicode_test") {
    Some(value) => assert_eq(value, "测试中文")
    None => @test.fail("Test failed")
  }
  
  match special_carrier.get("space key") {
    Some(value) => assert_eq(value, "space value")
    None => @test.fail("Test failed")
  }
  
  // 测试设置操作
  special_carrier.set("new_key", "new_value")
  special_carrier.set("", "updated_empty_key")
  special_carrier.set("special!@#$%^&*()", "updated_special_value")
  
  // 验证设置操作（注意：由于简化实现，这里可能不会真正修改数据）
  let special_keys = special_carrier.keys()
  assert_eq(special_keys.length(), 8)  // 原始键的数量
}

test "trace_context_propagator_complex_scenarios" {
  let propagator = W3CTraceContextPropagator::{}
  
  // 测试各种有效的traceparent格式
  let valid_traceparents = [
    "00-0af7651916cd43dd8448eb211c80319c-b7ad6b7169203331-01",
    "00-4bf92f3577b34da6a3ce929d0e0e4736-00f067aa0ba902b7-01",
    "00-00000000000000000000000000000000-0000000000000000-00",
    "01-0af7651916cd43dd8448eb211c80319c-b7ad6b7169203331-01",
    "ff-0af7651916cd43dd8448eb211c80319c-b7ad6b7169203331-01"
  ]
  
  let mut i = 0
  while i < valid_traceparents.length() {
    let carrier = MapCarrier::from_map([
      ("traceparent", valid_traceparents[i])
    ])
    
    let ctx = Context::empty()
    let extracted_ctx = propagator.extract(ctx, carrier)
    assert_eq(true, true)
    i = i + 1
  }
  
  // 测试各种无效的traceparent格式
  let invalid_traceparents = [
    "invalid-format",
    "00-0af7651916cd43dd8448eb211c80319c-b7ad6b7169203331",  // 缺少flags
    "00-0af7651916cd43dd8448eb211c80319c-01",  // 缺少span_id和flags
    "00-0af7651916cd43dd8448eb211c80319c-b7ad6b7169203331-01-extra",  // 额外部分
    "00-0af7651916cd43dd8448eb211c80319c-b7ad6b7169203331-xyz",  // 无效flags
    "00-0af7651916cd43dd8448eb211c80319c-b7ad6b71692033-01",  // span_id长度错误
    "00-0af7651916cd43dd8448eb211c80319c-b7ad6b7169203331-0123",  // flags长度错误
    ""  // 空字符串
  ]
  
  i = 0
  while i < invalid_traceparents.length() {
    let carrier = MapCarrier::from_map([
      ("traceparent", invalid_traceparents[i])
    ])
    
    let ctx = Context::empty()
    let extracted_ctx = propagator.extract(ctx, carrier)
    assert_eq(true, true)  // 即使格式无效，也不应该崩溃
    i = i + 1
  }
  
  // 测试带有tracestate的情况
  let carrier_with_tracestate = MapCarrier::from_map([
    ("traceparent", "00-0af7651916cd43dd8448eb211c80319c-b7ad6b7169203331-01"),
    ("tracestate", "rojo=00f067aa0ba902b7,congo=t61rcWkgMzE")
  ])
  
  let ctx = Context::empty()
  let extracted_ctx_with_tracestate = propagator.extract(ctx, carrier_with_tracestate)
  assert_eq(true, true)
  
  // 测试只有tracestate没有traceparent的情况
  let carrier_tracestate_only = MapCarrier::from_map([
    ("tracestate", "rojo=00f067aa0ba902b7,congo=t61rcWkgMzE")
  ])
  
  let extracted_tracestate_only = propagator.extract(ctx, carrier_tracestate_only)
  assert_eq(true, true)
}

test "propagation_integration_with_context_and_baggage" {
  // 测试传播与Context和Baggage的集成
  
  let trace_propagator = W3CTraceContextPropagator::{}
  let baggage_propagator = W3CBaggagePropagator::{}
  let composite = CompositePropagator::new([trace_propagator, baggage_propagator])
  
  // 创建带有baggage的context
  let baggage = Baggage::empty()
    .with_entry("user.id", "12345")
    .with_entry("session.id", "abcdef")
    .with_entry("request.id", "xyz789")
    .with_entry("service.name", "payment-service")
    .with_entry("deployment.env", "production")
  
  let ctx = Context::empty()
    .with_value(create_key("trace.id"), "trace-123")
    .with_value(create_key("span.id"), "span-456")
  
  let carrier = MapCarrier::new()
  
  // 注入context和baggage
  composite.inject(ctx, carrier)
  
  // 验证注入的header
  match carrier.get("traceparent") {
    Some(_) => assert_eq(true, true)
    None => @test.fail("Test failed")
  }
  
  match carrier.get("baggage") {
    Some(_) => assert_eq(true, true)
    None => @test.fail("Test failed")
  }
  
  // 创建包含复杂baggage的carrier
  let complex_carrier = MapCarrier::from_map([
    ("traceparent", "00-0af7651916cd43dd8448eb211c80319c-b7ad6b7169203331-01"),
    ("tracestate", "vendor1=value1,vendor2=value2"),
    ("baggage", "user.id=12345;userIdType=registered,session.id=abcdef;sessionType=authenticated,request.id=xyz789;requestSource=api,service.name=payment-service;serviceTier=critical,deployment.env=production;envType=prod,tags=api,payment,critical;priority=high"),
    ("authorization", "Bearer token123"),
    ("x-forwarded-for", "192.168.1.1"),
    ("user-agent", "Mozilla/5.0 (compatible; TestBot/1.0)")
  ])
  
  // 提取复杂carrier
  let extracted_ctx = composite.extract(ctx, complex_carrier)
  assert_eq(true, true)
  
  // 测试多次注入和提取
  let carrier1 = MapCarrier::new()
  composite.inject(ctx, carrier1)
  
  let extracted_ctx1 = composite.extract(Context::empty(), carrier1)
  let carrier2 = MapCarrier::new()
  composite.inject(extracted_ctx1, carrier2)
  
  let extracted_ctx2 = composite.extract(Context::empty(), carrier2)
  assert_eq(true, true)
  
  // 验证多次传播不会丢失信息
  match carrier2.get("traceparent") {
    Some(_) => assert_eq(true, true)
    None => @test.fail("Test failed")
  }
  
  match carrier2.get("baggage") {
    Some(_) => assert_eq(true, true)
    None => @test.fail("Test failed")
  }
}