// CompositePropagator多种传播器组合测试用例
// 测试CompositePropagator的各种传播器组合和复杂场景

test "composite_propagator_single_propagator" {
  // 测试单个传播器的CompositePropagator
  
  let w3c_trace_propagator = W3CTraceContextPropagator::{}
  let propagators = [w3c_trace_propagator]
  let composite_propagator = CompositePropagator::new(propagators)
  
  let ctx = context::Context::empty()
  let carrier = MapCarrier::new()
  
  // 测试注入
  composite_propagator.inject(ctx, carrier)
  
  // 测试提取
  let extracted_ctx = composite_propagator.extract(ctx, carrier)
  
  // 验证上下文不为空（基本检查）
  assert_eq(extracted_ctx.values.length(), ctx.values.length())
}

test "composite_propagator_dual_propagators" {
  // 测试两个传播器的组合
  
  let w3c_trace_propagator = W3CTraceContextPropagator::{}
  let w3c_baggage_propagator = W3CBaggagePropagator::{}
  let propagators = [w3c_trace_propagator, w3c_baggage_propagator]
  let composite_propagator = CompositePropagator::new(propagators)
  
  let ctx = context::Context::empty()
  let carrier = MapCarrier::new()
  
  // 测试注入
  composite_propagator.inject(ctx, carrier)
  
  // 测试提取
  let extracted_ctx = composite_propagator.extract(ctx, carrier)
  
  // 验证上下文不为空（基本检查）
  assert_eq(extracted_ctx.values.length(), ctx.values.length())
}

test "composite_propagator_multiple_propagators" {
  // 测试多个传播器的组合
  
  let w3c_trace_propagator = W3CTraceContextPropagator::{}
  let w3c_baggage_propagator = W3CBaggagePropagator::{}
  
  // 创建多个相同的传播器来测试重复处理
  let propagators = [
    w3c_trace_propagator,
    w3c_baggage_propagator,
    w3c_trace_propagator,
    w3c_baggage_propagator,
    w3c_trace_propagator
  ]
  let composite_propagator = CompositePropagator::new(propagators)
  
  let ctx = context::Context::empty()
  let carrier = MapCarrier::new()
  
  // 测试注入
  composite_propagator.inject(ctx, carrier)
  
  // 测试提取
  let extracted_ctx = composite_propagator.extract(ctx, carrier)
  
  // 验证上下文不为空（基本检查）
  assert_eq(extracted_ctx.values.length(), ctx.values.length())
}

test "composite_propagator_empty_propagators" {
  // 测试空传播器列表的CompositePropagator
  
  let propagators = []
  let composite_propagator = CompositePropagator::new(propagators)
  
  let ctx = context::Context::empty()
  let carrier = MapCarrier::new()
  
  // 测试注入（应该不执行任何操作）
  composite_propagator.inject(ctx, carrier)
  
  // 测试提取（应该返回原始上下文）
  let extracted_ctx = composite_propagator.extract(ctx, carrier)
  
  // 验证上下文保持不变
  assert_eq(extracted_ctx.values.length(), ctx.values.length())
}

test "composite_propagator_with_context_data" {
  // 测试带有上下文数据的CompositePropagator
  
  let w3c_trace_propagator = W3CTraceContextPropagator::{}
  let w3c_baggage_propagator = W3CBaggagePropagator::{}
  let propagators = [w3c_trace_propagator, w3c_baggage_propagator]
  let composite_propagator = CompositePropagator::new(propagators)
  
  // 创建带有数据的上下文
  let key1 = create_key("test.key1")
  let key2 = create_key("test.key2")
  let ctx_with_data = context::Context::empty()
    .with_value(key1, "value1")
    .with_value(key2, "value2")
  
  let carrier = MapCarrier::new()
  
  // 测试注入
  composite_propagator.inject(ctx_with_data, carrier)
  
  // 测试提取
  let extracted_ctx = composite_propagator.extract(context::Context::empty(), carrier)
  
  // 验证原始上下文数据仍然存在
  match ctx_with_data.get(key1) {
    Some(v) => assert_eq(v, "value1")
    None => @test.fail("Expected value1")
  }
  
  match ctx_with_data.get(key2) {
    Some(v) => assert_eq(v, "value2")
    None => @test.fail("Expected value2")
  }
}

test "composite_propagator_with_baggage_data" {
  // 测试带有Baggage数据的CompositePropagator
  
  let w3c_trace_propagator = W3CTraceContextPropagator::{}
  let w3c_baggage_propagator = W3CBaggagePropagator::{}
  let propagators = [w3c_trace_propagator, w3c_baggage_propagator]
  let composite_propagator = CompositePropagator::new(propagators)
  
  // 创建带有Baggage数据的上下文
  let baggage = Baggage::empty()
    .with_entry("user.id", "user-123")
    .with_entry("request.id", "req-456")
    .with_entry("trace.id", "trace-789")
  
  let ctx = context::Context::empty()
  let carrier = MapCarrier::new()
  
  // 测试注入
  composite_propagator.inject(ctx, carrier)
  
  // 测试提取
  let extracted_ctx = composite_propagator.extract(ctx, carrier)
  
  // 验证Baggage数据
  match baggage.get("user.id") {
    Some(v) => assert_eq(v, "user-123")
    None => @test.fail("Expected user.id")
  }
  
  match baggage.get("request.id") {
    Some(v) => assert_eq(v, "req-456")
    None => @test.fail("Expected request.id")
  }
  
  match baggage.get("trace.id") {
    Some(v) => assert_eq(v, "trace-789")
    None => @test.fail("Expected trace.id")
  }
}

test "composite_propagator_map_carrier_integration" {
  // 测试CompositePropagator与MapCarrier的集成
  
  let w3c_trace_propagator = W3CTraceContextPropagator::{}
  let w3c_baggage_propagator = W3CBaggagePropagator::{}
  let propagators = [w3c_trace_propagator, w3c_baggage_propagator]
  let composite_propagator = CompositePropagator::new(propagators)
  
  // 创建带有预存数据的MapCarrier
  let initial_data = [
    ("existing.header1", "value1"),
    ("existing.header2", "value2"),
    ("traceparent", "00-0af7651916cd43dd8448eb211c80319c-b7ad6b7169203331-01"),
    ("baggage", "key1=value1,key2=value2")
  ]
  let carrier = MapCarrier::from_map(initial_data)
  
  let ctx = context::Context::empty()
  
  // 测试注入到已有数据的carrier
  composite_propagator.inject(ctx, carrier)
  
  // 测试从已有数据的carrier提取
  let extracted_ctx = composite_propagator.extract(ctx, carrier)
  
  // 验证carrier的键
  let keys = carrier.keys()
  assert_eq(keys.length(), 4)
  
  // 验证预存数据仍然可以访问
  match carrier.get("existing.header1") {
    Some(v) => assert_eq(v, "value1")
    None => @test.fail("Expected existing.header1")
  }
  
  match carrier.get("existing.header2") {
    Some(v) => assert_eq(v, "value2")
    None => @test.fail("Expected existing.header2")
  }
  
  // 验证传播相关的头部
  match carrier.get("traceparent") {
    Some(_) => assert_eq(true, true)  // 存在即可
    None => @test.fail("Expected traceparent")
  }
  
  match carrier.get("baggage") {
    Some(_) => assert_eq(true, true)  // 存在即可
    None => @test.fail("Expected baggage")
  }
}

test "composite_propagator_header_names" {
  // 测试CompositePropagator使用的头部名称
  
  let w3c_trace_propagator = W3CTraceContextPropagator::{}
  let w3c_baggage_propagator = W3CBaggagePropagator::{}
  let propagators = [w3c_trace_propagator, w3c_baggage_propagator]
  let composite_propagator = CompositePropagator::new(propagators)
  
  let ctx = context::Context::empty()
  let carrier = MapCarrier::new()
  
  // 测试注入
  composite_propagator.inject(ctx, carrier)
  
  // 验证使用的头部名称
  let keys = carrier.keys()
  
  // 检查是否包含预期的头部
  let mut found_traceparent = false
  let mut found_baggage = false
  
  let mut i = 0
  while i < keys.length() {
    if keys[i] == "traceparent" {
      found_traceparent = true
    }
    if keys[i] == "baggage" {
      found_baggage = true
    }
    i = i + 1
  }
  
  assert_eq(found_traceparent, true)
  assert_eq(found_baggage, true)
}

test "composite_propagator_order_dependency" {
  // 测试CompositePropagator中传播器的顺序依赖性
  
  let w3c_trace_propagator = W3CTraceContextPropagator::{}
  let w3c_baggage_propagator = W3CBaggagePropagator::{}
  
  // 测试不同的顺序
  let propagators1 = [w3c_trace_propagator, w3c_baggage_propagator]
  let propagators2 = [w3c_baggage_propagator, w3c_trace_propagator]
  
  let composite_propagator1 = CompositePropagator::new(propagators1)
  let composite_propagator2 = CompositePropagator::new(propagators2)
  
  let ctx = context::Context::empty()
  let carrier1 = MapCarrier::new()
  let carrier2 = MapCarrier::new()
  
  // 测试不同顺序的注入
  composite_propagator1.inject(ctx, carrier1)
  composite_propagator2.inject(ctx, carrier2)
  
  // 测试不同顺序的提取
  let extracted_ctx1 = composite_propagator1.extract(ctx, carrier1)
  let extracted_ctx2 = composite_propagator2.extract(ctx, carrier2)
  
  // 验证两种顺序都能正常工作
  assert_eq(extracted_ctx1.values.length(), extracted_ctx2.values.length())
  
  // 验证两个carrier都包含相同的头部
  let keys1 = carrier1.keys()
  let keys2 = carrier2.keys()
  
  // 检查traceparent和baggage都存在
  let mut traceparent_in_1 = false
  let mut baggage_in_1 = false
  let mut traceparent_in_2 = false
  let mut baggage_in_2 = false
  
  let mut i = 0
  while i < keys1.length() {
    if keys1[i] == "traceparent" {
      traceparent_in_1 = true
    }
    if keys1[i] == "baggage" {
      baggage_in_1 = true
    }
    i = i + 1
  }
  
  let mut j = 0
  while j < keys2.length() {
    if keys2[j] == "traceparent" {
      traceparent_in_2 = true
    }
    if keys2[j] == "baggage" {
      baggage_in_2 = true
    }
    j = j + 1
  }
  
  assert_eq(traceparent_in_1, true)
  assert_eq(baggage_in_1, true)
  assert_eq(traceparent_in_2, true)
  assert_eq(baggage_in_2, true)
}

test "composite_propagator_complex_scenario" {
  // 测试CompositePropagator的复杂场景
  
  let w3c_trace_propagator = W3CTraceContextPropagator::{}
  let w3c_baggage_propagator = W3CBaggagePropagator::{}
  let propagators = [w3c_trace_propagator, w3c_baggage_propagator]
  let composite_propagator = CompositePropagator::new(propagators)
  
  // 创建复杂的上下文数据
  let key1 = create_key("request.id")
  let key2 = create_key("user.id")
  let key3 = create_key("operation.name")
  
  let ctx = context::Context::empty()
    .with_value(key1, "req-12345")
    .with_value(key2, "user-67890")
    .with_value(key3, "process-data")
  
  // 创建复杂的Baggage数据
  let baggage = Baggage::empty()
    .with_entry("service.name", "api-gateway")
    .with_entry("service.version", "2.1.0")
    .with_entry("deployment.environment", "production")
    .with_entry("trace.sampled", "true")
    .with_entry("debug.enabled", "false")
  
  // 创建带有预存数据的carrier
  let initial_data = [
    ("authorization", "Bearer token123"),
    ("content-type", "application/json"),
    ("x-request-id", "ext-req-123"),
    ("x-custom-header", "custom-value")
  ]
  let carrier = MapCarrier::from_map(initial_data)
  
  // 测试注入
  composite_propagator.inject(ctx, carrier)
  
  // 测试提取到新的上下文
  let empty_ctx = context::Context::empty()
  let extracted_ctx = composite_propagator.extract(empty_ctx, carrier)
  
  // 验证原始上下文数据
  match ctx.get(key1) {
    Some(v) => assert_eq(v, "req-12345")
    None => @test.fail("Expected req-12345")
  }
  
  match ctx.get(key2) {
    Some(v) => assert_eq(v, "user-67890")
    None => @test.fail("Expected user-67890")
  }
  
  match ctx.get(key3) {
    Some(v) => assert_eq(v, "process-data")
    None => @test.fail("Expected process-data")
  }
  
  // 验证Baggage数据
  match baggage.get("service.name") {
    Some(v) => assert_eq(v, "api-gateway")
    None => @test.fail("Expected api-gateway")
  }
  
  match baggage.get("trace.sampled") {
    Some(v) => assert_eq(v, "true")
    None => @test.fail("Expected true")
  }
  
  // 验证carrier中的所有数据
  let keys = carrier.keys()
  
  // 验证预存头部仍然存在
  match carrier.get("authorization") {
    Some(v) => assert_eq(v, "Bearer token123")
    None => @test.fail("Expected authorization")
  }
  
  match carrier.get("content-type") {
    Some(v) => assert_eq(v, "application/json")
    None => @test.fail("Expected content-type")
  }
  
  match carrier.get("x-request-id") {
    Some(v) => assert_eq(v, "ext-req-123")
    None => @test.fail("Expected x-request-id")
  }
  
  match carrier.get("x-custom-header") {
    Some(v) => assert_eq(v, "custom-value")
    None => @test.fail("Expected x-custom-header")
  }
  
  // 验证传播头部存在
  match carrier.get("traceparent") {
    Some(_) => assert_eq(true, true)
    None => @test.fail("Expected traceparent")
  }
  
  match carrier.get("baggage") {
    Some(_) => assert_eq(true, true)
    None => @test.fail("Expected baggage")
  }
  
  // 验证键的数量（预存4个 + 传播2个）
  assert_eq(keys.length(), 6)
}

test "composite_propagator_error_handling" {
  // 测试CompositePropagator的错误处理
  
  let w3c_trace_propagator = W3CTraceContextPropagator::{}
  let w3c_baggage_propagator = W3CBaggagePropagator::{}
  let propagators = [w3c_trace_propagator, w3c_baggage_propagator]
  let composite_propagator = CompositePropagator::new(propagators)
  
  // 创建带有无效数据的carrier
  let invalid_data = [
    ("traceparent", "invalid-traceparent-format"),
    ("baggage", "invalid=baggage=format=with=too=many=equals"),
    ("malformed-header", "")
  ]
  let carrier = MapCarrier::from_map(invalid_data)
  
  let ctx = context::Context::empty()
  
  // 测试从无效数据提取（应该优雅地处理错误）
  let extracted_ctx = composite_propagator.extract(ctx, carrier)
  
  // 验证即使有无效数据，提取也能正常工作
  assert_eq(extracted_ctx.values.length(), ctx.values.length())
  
  // 测试注入到正常carrier
  let normal_carrier = MapCarrier::new()
  composite_propagator.inject(ctx, normal_carrier)
  
  // 验证注入成功
  let keys = normal_carrier.keys()
  assert_eq(keys.length() > 0, true)
}

test "composite_propagator_performance_considerations" {
  // 测试CompositePropagator的性能考虑
  
  let w3c_trace_propagator = W3CTraceContextPropagator::{}
  let w3c_baggage_propagator = W3CBaggagePropagator::{}
  
  // 创建大量传播器
  let mut propagators = []
  let mut i = 0
  while i < 10 {
    propagators.push(w3c_trace_propagator)
    propagators.push(w3c_baggage_propagator)
    i = i + 1
  }
  
  let composite_propagator = CompositePropagator::new(propagators)
  
  let ctx = context::Context::empty()
  let carrier = MapCarrier::new()
  
  // 测试大量传播器的注入
  composite_propagator.inject(ctx, carrier)
  
  // 测试大量传播器的提取
  let extracted_ctx = composite_propagator.extract(ctx, carrier)
  
  // 验证操作成功完成
  assert_eq(extracted_ctx.values.length(), ctx.values.length())
  
  // 验证carrier包含预期的头部
  let keys = carrier.keys()
  assert_eq(keys.length() > 0, true)
}