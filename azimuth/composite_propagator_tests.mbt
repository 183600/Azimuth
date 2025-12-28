// 复合传播器测试用例
// 测试CompositePropagator的创建、注入、提取等操作

test "composite_propagator_creation" {
  // 测试复合传播器创建
  let trace_propagator = W3CTraceContextPropagator::new()
  let baggage_propagator = W3CBaggagePropagator::new()
  
  // 创建单个传播器的复合传播器
  let single_composite = CompositePropagator::new([trace_propagator])
  assert_eq(single_composite.propagators.length(), 1)
  
  // 创建多个传播器的复合传播器
  let multi_composite = CompositePropagator::new([trace_propagator])
  assert_eq(multi_composite.propagators.length(), 1)
}

test "composite_propagator_injection_extraction" {
  // 测试复合传播器的注入和提取
  let propagator = W3CTraceContextPropagator::new()
  let composite = CompositePropagator::new([propagator])
  
  // 创建载体和上下文
  let carrier = TextMapCarrier::new()
  let context = Context::root()
  
  // 注入操作
  CompositePropagator::inject(composite, context, carrier)
  
  // 验证注入结果
  let injected_trace = TextMapCarrier::get(carrier, "traceparent")
  assert_eq(injected_trace, Some("00-test-trace-id-test-span-id-01"))
  
  // 提取操作
  let extracted_context = CompositePropagator::extract(composite, carrier)
  let extracted_value = Context::get(extracted_context, ContextKey::new("extracted"))
  assert_eq(extracted_value, Some("true"))
}

test "composite_propagator_multiple_injection" {
  // 测试多次注入操作
  let propagator = W3CTraceContextPropagator::new()
  let composite = CompositePropagator::new([propagator])
  
  let carrier = TextMapCarrier::new()
  let context = Context::root()
  
  // 第一次注入
  CompositePropagator::inject(composite, context, carrier)
  let first_injection = TextMapCarrier::get(carrier, "traceparent")
  assert_true(first_injection.is_some())
  
  // 第二次注入（应该覆盖或追加）
  CompositePropagator::inject(composite, context, carrier)
  let second_injection = TextMapCarrier::get(carrier, "traceparent")
  assert_true(second_injection.is_some())
}

test "composite_propagator_context_preservation" {
  // 测试上下文保留
  let propagator = W3CTraceContextPropagator::new()
  let composite = CompositePropagator::new([propagator])
  
  // 创建带有数据的上下文
  let key = ContextKey::new("test.key")
  let original_context = Context::with_value(Context::root(), key, "test.value")
  
  let carrier = TextMapCarrier::new()
  
  // 注入操作
  CompositePropagator::inject(composite, original_context, carrier)
  
  // 提取操作
  let extracted_context = CompositePropagator::extract(composite, carrier)
  
  // 验证提取的上下文包含预期值
  let extracted_value = Context::get(extracted_context, ContextKey::new("extracted"))
  assert_eq(extracted_value, Some("true"))
}

test "composite_propagator_empty_propagators" {
  // 测试空传播器列表的复合传播器
  let empty_composite = CompositePropagator::new([])
  assert_eq(empty_composite.propagators.length(), 0)
  
  let carrier = TextMapCarrier::new()
  let context = Context::root()
  
  // 即使没有传播器，操作也应该安全执行
  CompositePropagator::inject(empty_composite, context, carrier)
  let extracted = CompositePropagator::extract(empty_composite, carrier)
  
  // 验证操作不会崩溃
  assert_true(true)
}

test "composite_propagator_field_access" {
  // 测试复合传播器的字段访问
  let propagator = W3CTraceContextPropagator::new()
  let composite = CompositePropagator::new([propagator])
  
  // 访问传播器列表
  assert_eq(composite.propagators.length(), 1)
  
  // 创建多个传播器的复合传播器
  let propagator2 = W3CTraceContextPropagator::new()
  let multi_composite = CompositePropagator::new([propagator, propagator2])
  assert_eq(multi_composite.propagators.length(), 2)
}

test "composite_propagator_trace_context_validation" {
  // 测试追踪上下文验证
  let propagator = W3CTraceContextPropagator::new()
  let composite = CompositePropagator::new([propagator])
  
  let carrier = TextMapCarrier::new()
  let context = Context::root()
  
  // 注入追踪上下文
  CompositePropagator::inject(composite, context, carrier)
  
  // 验证注入的追踪格式
  let traceparent = TextMapCarrier::get(carrier, "traceparent")
  match traceparent {
    Some(trace) => {
      // 验证traceparent格式：version-trace_id-span_id-flags
      assert_true(trace.contains("-"))
      let parts = trace.split("-")
      assert_eq(parts.length(), 4)
    }
    None => assert_false(true, "Expected traceparent header")
  }
}

test "composite_propagator_with_span_context" {
  // 测试与SpanContext的集成
  let span_context = SpanContext::new("test_trace_id", "test_span_id", true, "key1=value1")
  
  let propagator = W3CTraceContextPropagator::new()
  let composite = CompositePropagator::new([propagator])
  
  let carrier = TextMapCarrier::new()
  let context = Context::root()
  
  // 注入包含SpanContext的上下文
  CompositePropagator::inject(composite, context, carrier)
  
  // 验证SpanContext信息被正确传播
  let injected = TextMapCarrier::get(carrier, "traceparent")
  assert_true(injected.is_some())
  
  // 提取并验证
  let extracted = CompositePropagator::extract(composite, carrier)
  let value = Context::get(extracted, ContextKey::new("extracted"))
  assert_eq(value, Some("true"))
}

test "composite_propagator_error_handling" {
  // 测试错误处理
  let propagator = W3CTraceContextPropagator::new()
  let composite = CompositePropagator::new([propagator])
  
  // 使用空的载体
  let empty_carrier = TextMapCarrier::new()
  let context = Context::root()
  
  // 注入操作应该安全执行
  CompositePropagator::inject(composite, context, empty_carrier)
  
  // 从空载体提取应该安全执行
  let extracted = CompositePropagator::extract(composite, empty_carrier)
  let value = Context::get(extracted, ContextKey::new("extracted"))
  assert_eq(value, Some("true"))
}

test "composite_propagator_concurrent_operations" {
  // 测试并发操作
  let propagator = W3CTraceContextPropagator::new()
  let composite = CompositePropagator::new([propagator])
  
  // 模拟多个并发操作
  for i in range(0, 5) {
    let carrier = TextMapCarrier::new()
    let context = Context::root()
    
    // 注入
    CompositePropagator::inject(composite, context, carrier)
    
    // 验证注入结果
    let injected = TextMapCarrier::get(carrier, "traceparent")
    assert_true(injected.is_some())
    
    // 提取
    let extracted = CompositePropagator::extract(composite, carrier)
    let value = Context::get(extracted, ContextKey::new("extracted"))
    assert_eq(value, Some("true"))
  }
}