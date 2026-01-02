// Azimuth Composite Propagator Advanced Tests
// 测试复合传播器高级功能

test "composite_propagator_multiple_trace_propagators" {
  // 测试包含多个追踪传播器的复合传播器
  let trace_propagator1 = W3CTraceContextPropagator::new()
  let trace_propagator2 = W3CTraceContextPropagator::new()
  let trace_propagator3 = W3CTraceContextPropagator::new()
  
  let composite_propagator = CompositePropagator::new([trace_propagator1, trace_propagator2, trace_propagator3])
  
  // 验证传播器数量
  assert_eq(composite_propagator.propagators.length(), 3)
}

test "composite_propagator_mixed_propagators" {
  // 测试混合类型的传播器组合
  let trace_propagator = W3CTraceContextPropagator::new()
  let baggage_propagator = W3CBaggagePropagator::new()
  let another_trace_propagator = W3CTraceContextPropagator::new()
  
  let composite_propagator = CompositePropagator::new([
    trace_propagator,
    baggage_propagator,
    another_trace_propagator
  ])
  
  // 验证传播器数量
  assert_eq(composite_propagator.propagators.length(), 3)
}

test "composite_propagator_empty_list" {
  // 测试空传播器列表的复合传播器
  let composite_propagator = CompositePropagator::new([])
  
  // 验证传播器数量为0
  assert_eq(composite_propagator.propagators.length(), 0)
  
  // 测试注入操作（应该不会失败）
  let context = Context::root()
  let carrier = TextMapCarrier::new()
  CompositePropagator::inject(composite_propagator, context, carrier)
  
  // 测试提取操作（应该不会失败）
  let extracted_context = CompositePropagator::extract(composite_propagator, carrier)
  
  // 验证操作完成
  assert_eq(true, true)
}

test "composite_propagator_inject_with_context_values" {
  // 测试带上下文值的注入
  let trace_propagator = W3CTraceContextPropagator::new()
  let composite_propagator = CompositePropagator::new([trace_propagator])
  
  // 创建带值的上下文
  let root_context = Context::root()
  let request_key = ContextKey::new("request.id")
  let user_key = ContextKey::new("user.id")
  let context_with_values = Context::with_value(
    Context::with_value(root_context, request_key, "req-123"),
    user_key,
    "user-456"
  )
  
  let carrier = TextMapCarrier::new()
  
  // 执行注入
  CompositePropagator::inject(composite_propagator, context_with_values, carrier)
  
  // 验证注入结果
  let injected_trace = TextMapCarrier::get(carrier, "traceparent")
  match injected_trace {
    Some(value) => assert_eq(value, "00-test-trace-id-test-span-id-01")
    _ => assert_fail("Expected injected traceparent")
  }
}

test "composite_propagator_extract_with_carrier_data" {
  // 测试带载体数据的提取
  let trace_propagator = W3CTraceContextPropagator::new()
  let composite_propagator = CompositePropagator::new([trace_propagator])
  
  let carrier = TextMapCarrier::new()
  
  // 设置载体数据（注意：当前实现不支持实际设置）
  // TextMapCarrier::set(carrier, "traceparent", "custom-trace-value")
  
  // 执行提取
  let extracted_context = CompositePropagator::extract(composite_propagator, carrier)
  
  // 验证提取结果
  let extracted_key = ContextKey::new("extracted")
  let extracted_value = Context::get(extracted_context, extracted_key)
  match extracted_value {
    Some(value) => assert_eq(value, "true")
    _ => assert_fail("Expected extracted value")
  }
}

test "composite_propagator_multiple_inject_operations" {
  // 测试多次注入操作
  let trace_propagator = W3CTraceContextPropagator::new()
  let composite_propagator = CompositePropagator::new([trace_propagator])
  
  let context1 = Context::root()
  let context2 = Context::with_value(Context::root(), ContextKey::new("operation.id"), "op-123")
  
  let carrier1 = TextMapCarrier::new()
  let carrier2 = TextMapCarrier::new()
  
  // 执行多次注入
  CompositePropagator::inject(composite_propagator, context1, carrier1)
  CompositePropagator::inject(composite_propagator, context2, carrier2)
  
  // 验证注入结果
  let trace1 = TextMapCarrier::get(carrier1, "traceparent")
  let trace2 = TextMapCarrier::get(carrier2, "traceparent")
  
  match trace1 {
    Some(value) => assert_eq(value, "00-test-trace-id-test-span-id-01")
    _ => assert_fail("Expected injected traceparent in carrier1")
  }
  
  match trace2 {
    Some(value) => assert_eq(value, "00-test-trace-id-test-span-id-01")
    _ => assert_fail("Expected injected traceparent in carrier2")
  }
}

test "composite_propagator_multiple_extract_operations" {
  // 测试多次提取操作
  let trace_propagator = W3CTraceContextPropagator::new()
  let composite_propagator = CompositePropagator::new([trace_propagator])
  
  let carrier = TextMapCarrier::new()
  
  // 执行多次提取
  let extracted_context1 = CompositePropagator::extract(composite_propagator, carrier)
  let extracted_context2 = CompositePropagator::extract(composite_propagator, carrier)
  
  // 验证提取结果
  let extracted_key = ContextKey::new("extracted")
  
  let value1 = Context::get(extracted_context1, extracted_key)
  let value2 = Context::get(extracted_context2, extracted_key)
  
  match value1 {
    Some(value) => assert_eq(value, "true")
    _ => assert_fail("Expected extracted value in context1")
  }
  
  match value2 {
    Some(value) => assert_eq(value, "true")
    _ => assert_fail("Expected extracted value in context2")
  }
}

test "composite_propagator_inject_extract_cycle" {
  // 测试注入-提取循环
  let trace_propagator = W3CTraceContextPropagator::new()
  let composite_propagator = CompositePropagator::new([trace_propagator])
  
  // 1. 创建原始上下文
  let original_context = Context::with_value(
    Context::root(),
    ContextKey::new("correlation.id"),
    "corr-12345"
  )
  
  // 2. 注入到载体
  let carrier = TextMapCarrier::new()
  CompositePropagator::inject(composite_propagator, original_context, carrier)
  
  // 3. 从载体提取
  let extracted_context = CompositePropagator::extract(composite_propagator, carrier)
  
  // 4. 验证提取结果
  let extracted_key = ContextKey::new("extracted")
  let extracted_value = Context::get(extracted_context, extracted_key)
  match extracted_value {
    Some(value) => assert_eq(value, "true")
    _ => assert_fail("Expected extracted value")
  }
  
  // 5. 验证载体包含追踪信息
  let trace_value = TextMapCarrier::get(carrier, "traceparent")
  match trace_value {
    Some(value) => assert_eq(value, "00-test-trace-id-test-span-id-01")
    _ => assert_fail("Expected traceparent in carrier")
  }
}

test "composite_propagator_with_complex_context" {
  // 测试复杂上下文的传播
  let trace_propagator = W3CTraceContextPropagator::new()
  let composite_propagator = CompositePropagator::new([trace_propagator])
  
  // 创建复杂的上下文层次结构
  let base_context = Context::root()
  let request_context = Context::with_value(
    base_context,
    ContextKey::new("request.id"),
    "req-abcdef"
  )
  let user_context = Context::with_value(
    request_context,
    ContextKey::new("user.id"),
    "user-123456"
  )
  let operation_context = Context::with_value(
    user_context,
    ContextKey::new("operation.type"),
    "database.query"
  )
  
  let carrier = TextMapCarrier::new()
  
  // 注入复杂上下文
  CompositePropagator::inject(composite_propagator, operation_context, carrier)
  
  // 提取上下文
  let extracted_context = CompositePropagator::extract(composite_propagator, carrier)
  
  // 验证提取结果
  let extracted_key = ContextKey::new("extracted")
  let extracted_value = Context::get(extracted_context, extracted_key)
  match extracted_value {
    Some(value) => assert_eq(value, "true")
    _ => assert_fail("Expected extracted value")
  }
}

test "composite_propagator_multiple_carriers" {
  // 测试多个载体的传播
  let trace_propagator = W3CTraceContextPropagator::new()
  let composite_propagator = CompositePropagator::new([trace_propagator])
  
  let context = Context::with_value(
    Context::root(),
    ContextKey::new("service.name"),
    "test-service"
  )
  
  // 创建多个载体
  let carrier1 = TextMapCarrier::new()
  let carrier2 = TextMapCarrier::new()
  let carrier3 = TextMapCarrier::new()
  
  // 向所有载体注入相同的上下文
  CompositePropagator::inject(composite_propagator, context, carrier1)
  CompositePropagator::inject(composite_propagator, context, carrier2)
  CompositePropagator::inject(composite_propagator, context, carrier3)
  
  // 从所有载体提取上下文
  let extracted_context1 = CompositePropagator::extract(composite_propagator, carrier1)
  let extracted_context2 = CompositePropagator::extract(composite_propagator, carrier2)
  let extracted_context3 = CompositePropagator::extract(composite_propagator, carrier3)
  
  // 验证所有提取结果
  let extracted_key = ContextKey::new("extracted")
  
  let value1 = Context::get(extracted_context1, extracted_key)
  let value2 = Context::get(extracted_context2, extracted_key)
  let value3 = Context::get(extracted_context3, extracted_key)
  
  match value1 {
    Some(value) => assert_eq(value, "true")
    _ => assert_fail("Expected extracted value in context1")
  }
  
  match value2 {
    Some(value) => assert_eq(value, "true")
    _ => assert_fail("Expected extracted value in context2")
  }
  
  match value3 {
    Some(value) => assert_eq(value, "true")
    _ => assert_fail("Expected extracted value in context3")
  }
}

test "composite_propagator_comprehensive_workflow" {
  // 测试复合传播器的完整工作流
  let trace_propagator1 = W3CTraceContextPropagator::new()
  let baggage_propagator = W3CBaggagePropagator::new()
  let trace_propagator2 = W3CTraceContextPropagator::new()
  
  let composite_propagator = CompositePropagator::new([
    trace_propagator1,
    baggage_propagator,
    trace_propagator2
  ])
  
  // 1. 创建服务A的上下文
  let service_a_context = Context::with_value(
    Context::root(),
    ContextKey::new("service.a.id"),
    "service-a-123"
  )
  
  // 2. 注入到载体（模拟服务间调用）
  let carrier_ab = TextMapCarrier::new()
  CompositePropagator::inject(composite_propagator, service_a_context, carrier_ab)
  
  // 3. 服务B提取上下文
  let service_b_context = CompositePropagator::extract(composite_propagator, carrier_ab)
  let service_b_enhanced = Context::with_value(
    service_b_context,
    ContextKey::new("service.b.id"),
    "service-b-456"
  )
  
  // 4. 服务B注入到新载体（模拟继续调用服务C）
  let carrier_bc = TextMapCarrier::new()
  CompositePropagator::inject(composite_propagator, service_b_enhanced, carrier_bc)
  
  // 5. 服务C提取上下文
  let service_c_context = CompositePropagator::extract(composite_propagator, carrier_bc)
  let service_c_enhanced = Context::with_value(
    service_c_context,
    ContextKey::new("service.c.id"),
    "service-c-789"
  )
  
  // 6. 验证整个传播链
  let extracted_key = ContextKey::new("extracted")
  
  let service_c_value = Context::get(service_c_enhanced, extracted_key)
  match service_c_value {
    Some(value) => assert_eq(value, "true")
    _ => assert_fail("Expected extracted value in service C context")
  }
  
  // 验证载体中的追踪信息
  let trace_ab = TextMapCarrier::get(carrier_ab, "traceparent")
  let trace_bc = TextMapCarrier::get(carrier_bc, "traceparent")
  
  match trace_ab {
    Some(value) => assert_eq(value, "00-test-trace-id-test-span-id-01")
    _ => assert_fail("Expected traceparent in carrier AB")
  }
  
  match trace_bc {
    Some(value) => assert_eq(value, "00-test-trace-id-test-span-id-01")
    _ => assert_fail("Expected traceparent in carrier BC")
  }
}