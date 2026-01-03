// 复合传播器的高级注入和提取测试
// 测试复合传播器在复杂场景下的注入和提取功能

test "复合传播器的多协议支持" {
  // 测试复合传播器支持多种传播协议
  let trace_propagator = W3CTraceContextPropagator::new()
  let baggage_propagator = W3CBaggagePropagator::new()
  let custom_propagator = CustomTracePropagator::new("x-custom-trace")
  let correlation_propagator = CorrelationContextPropagator::new()
  
  // 创建包含多种协议的复合传播器
  let composite = CompositePropagator::multi_protocol([
    trace_propagator,
    baggage_propagator,
    custom_propagator,
    correlation_propagator
  ])
  
  // 准备上下文数据
  let span_ctx = SpanContext::new("multi-protocol-trace", "multi-protocol-span", true, "key=value")
  let baggage = Baggage::new()
  Baggage::set_entry(baggage, "user.id", "12345")
  Baggage::set_entry(baggage, "request.id", "req-67890")
  
  let correlation_data = [
    ("correlation.id", "corr-12345"),
    ("session.id", "sess-67890"),
    ("tenant.id", "tenant-001")
  ]
  
  let carrier = TextMapCarrier::new()
  
  // 使用复合传播器注入所有上下文
  CompositePropagator::inject_all(composite, carrier, span_ctx, baggage, correlation_data)
  
  // 验证所有协议的头部都被注入
  let headers = TextMapCarrier::headers(carrier)
  let expected_headers = ["traceparent", "baggage", "x-custom-trace", "correlation-context"]
  
  for expected_header in expected_headers {
    let found = false
    for header in headers {
      if header.0 == expected_header {
        found = true
        break
      }
    }
    assert_true(found, "Should have header: " + expected_header)
  }
  
  // 提取并验证所有上下文
  let extracted_data = CompositePropagator::extract_all(composite, carrier)
  
  assert_eq(extracted_data.span_ctx.trace_id, "multi-protocol-trace")
  assert_eq(Baggage::get_entry(extracted_data.baggage, "user.id"), Some("12345"))
  assert_eq(extracted_data.correlation_data.get("correlation.id"), Some("corr-12345"))
}

test "复合传播器的优先级处理" {
  // 测试复合传播器中不同传播器的优先级处理
  let high_priority_propagator = PriorityTracePropagator::new("high", 100)
  let medium_priority_propagator = PriorityTracePropagator::new("medium", 50)
  let low_priority_propagator = PriorityTracePropagator::new("low", 10)
  
  // 创建带优先级的复合传播器
  let composite = CompositePropagator::with_priority([
    high_priority_propagator,
    medium_priority_propagator,
    low_priority_propagator
  ])
  
  let span_ctx = SpanContext::new("priority-trace", "priority-span", true, "")
  let carrier = TextMapCarrier::new()
  
  // 注入上下文
  CompositePropagator::inject(composite, span_ctx, carrier)
  
  // 验证高优先级传播器的头部被优先使用
  let high_header = TextMapCarrier::get(carrier, "x-high-priority-trace")
  let medium_header = TextMapCarrier::get(carrier, "x-medium-priority-trace")
  let low_header = TextMapCarrier::get(carrier, "x-low-priority-trace")
  
  assert_true(high_header.length > 0, "High priority header should be present")
  
  // 提取时验证优先级处理
  let extracted_ctx = CompositePropagator::extract_with_priority(composite, carrier)
  assert_eq(SpanContext::trace_id(extracted_ctx), "priority-trace")
}

test "复合传播器的条件注入" {
  // 测试基于条件的注入功能
  let trace_propagator = W3CTraceContextPropagator::new()
  let baggage_propagator = W3CBaggagePropagator::new()
  
  // 创建条件复合传播器
  let composite = CompositePropagator::conditional([
    (trace_propagator, fn(ctx) { SpanContext::is_sampled(ctx) }),
    (baggage_propagator, fn(ctx) { SpanContext::trace_state(ctx).contains("baggage=true") })
  ])
  
  // 测试采样条件
  let sampled_ctx = SpanContext::new("conditional-trace", "conditional-span", true, "baggage=true")
  let unsampled_ctx = SpanContext::new("conditional-trace-2", "conditional-span-2", false, "baggage=true")
  
  let carrier1 = TextMapCarrier::new()
  let carrier2 = TextMapCarrier::new()
  
  // 注入采样和非采样上下文
  CompositePropagator::inject_conditional(composite, sampled_ctx, carrier1)
  CompositePropagator::inject_conditional(composite, unsampled_ctx, carrier2)
  
  // 验证条件注入结果
  let traceparent1 = TextMapCarrier::get(carrier1, "traceparent")
  let traceparent2 = TextMapCarrier::get(carrier2, "traceparent")
  let baggage1 = TextMapCarrier::get(carrier1, "baggage")
  let baggage2 = TextMapCarrier::get(carrier2, "baggage")
  
  assert_true(traceparent1.length > 0, "Sampled context should have traceparent")
  assert_true(traceparent2.length == 0, "Unsampled context should not have traceparent")
  assert_true(baggage1.length > 0, "Context with baggage flag should have baggage header")
  assert_true(baggage2.length > 0, "Context with baggage flag should have baggage header")
}

test "复合传播器的转换和映射" {
  // 测试传播过程中的数据转换和映射
  let legacy_propagator = LegacyTracePropagator::new()
  let modern_propagator = W3CTraceContextPropagator::new()
  
  // 创建带转换功能的复合传播器
  let composite = CompositePropagator::with_transformation([
    (legacy_propagator, modern_propagator, LegacyToModernConverter::new())
  ])
  
  // 创建传统格式的上下文
  let legacy_trace_id = "legacy-trace-12345"
  let legacy_span_id = "legacy-span-67890"
  let legacy_carrier = TextMapCarrier::new()
  
  TextMapCarrier::set(legacy_carrier, "x-trace-id", legacy_trace_id)
  TextMapCarrier::set(legacy_carrier, "x-span-id", legacy_span_id)
  
  // 使用复合传播器提取并转换
  let modern_ctx = CompositePropagator::extract_with_transformation(composite, legacy_carrier)
  
  // 验证转换结果
  assert_eq(SpanContext::trace_id(modern_ctx), legacy_trace_id)
  assert_true(SpanContext::is_valid(modern_ctx))
  
  // 反向转换测试
  let modern_carrier = TextMapCarrier::new()
  CompositePropagator::inject_with_reverse_transformation(composite, modern_ctx, modern_carrier)
  
  let converted_trace_id = TextMapCarrier::get(modern_carrier, "x-trace-id")
  let converted_span_id = TextMapCarrier::get(modern_carrier, "x-span-id")
  
  assert_eq(converted_trace_id, Some(legacy_trace_id))
  assert_eq(converted_span_id, Some(legacy_span_id))
}

test "复合传播器的错误处理和恢复" {
  // 测试传播过程中的错误处理和恢复机制
  let reliable_propagator = ReliableTracePropagator::new()
  let unreliable_propagator = UnreliableTracePropagator::new() // 会随机失败
  let fallback_propagator = FallbackTracePropagator::new()
  
  // 创建带错误恢复的复合传播器
  let composite = CompositePropagator::with_error_recovery([
    unreliable_propagator,
    reliable_propagator,
    fallback_propagator
  ])
  
  let span_ctx = SpanContext::new("error-recovery-trace", "error-recovery-span", true, "")
  let carrier = TextMapCarrier::new()
  
  // 注入上下文（即使有传播器失败，也应该成功）
  let inject_result = CompositePropagator::inject_with_recovery(composite, span_ctx, carrier)
  
  assert_true(inject_result.success, "Injection should succeed despite failures")
  assert_true(inject_result.failed_propagators.length > 0, "Should record failed propagators")
  
  // 验证至少有一个传播器成功
  let headers = TextMapCarrier::headers(carrier)
  assert_true(headers.length > 0, "Should have at least one successful injection")
  
  // 提取测试
  let extract_result = CompositePropagator::extract_with_recovery(composite, carrier)
  assert_true(extract_result.success, "Extraction should succeed despite failures")
  assert_true(SpanContext::is_valid(extract_result.context), "Extracted context should be valid")
}

test "复合传播器的性能优化" {
  // 测试复合传播器的性能优化
  let propagators = []
  for i in 0..10 {
    propagators.push(W3CTraceContextPropagator::new())
  }
  
  let composite = CompositePropagator::optimized(propagators)
  let span_ctx = SpanContext::new("performance-trace", "performance-span", true, "")
  
  // 测试批量注入性能
  let carriers = []
  for i in 0..100 {
    carriers.push(TextMapCarrier::new())
  }
  
  let start_time = Clock::now_unix_nanos(Clock::system())
  
  for carrier in carriers {
    CompositePropagator::inject_optimized(composite, span_ctx, carrier)
  }
  
  let end_time = Clock::now_unix_nanos(Clock::system())
  let inject_duration = end_time - start_time
  
  assert_true(inject_duration < 500000000L, "Batch injection should complete within 0.5 seconds")
  
  // 测试批量提取性能
  start_time = Clock::now_unix_nanos(Clock::system())
  
  for carrier in carriers {
    CompositePropagator::extract_optimized(composite, carrier)
  }
  
  end_time = Clock::now_unix_nanos(Clock::system())
  let extract_duration = end_time - start_time
  
  assert_true(extract_duration < 500000000L, "Batch extraction should complete within 0.5 seconds")
}

test "复合传播器的动态配置" {
  // 测试运行时动态配置传播器
  let base_propagators = [
    W3CTraceContextPropagator::new(),
    W3CBaggagePropagator::new()
  ]
  
  let dynamic_composite = CompositePropagator::dynamic(base_propagators)
  
  let span_ctx = SpanContext::new("dynamic-trace", "dynamic-span", true, "")
  let baggage = Baggage::new()
  Baggage::set_entry(baggage, "dynamic.key", "dynamic.value")
  
  let carrier = TextMapCarrier::new()
  
  // 初始注入
  CompositePropagator::inject(dynamic_composite, span_ctx, carrier)
  
  // 动态添加新的传播器
  let custom_propagator = CustomTracePropagator::new("x-dynamic-trace")
  CompositePropagator::add_propagator(dynamic_composite, custom_propagator)
  
  // 再次注入
  CompositePropagator::inject(dynamic_composite, span_ctx, carrier)
  
  // 验证新传播器生效
  let dynamic_header = TextMapCarrier::get(carrier, "x-dynamic-trace")
  assert_true(dynamic_header.length > 0, "Dynamic propagator should be effective")
  
  // 动态移除传播器
  CompositePropagator::remove_propagator(dynamic_composite, custom_propagator)
  
  let new_carrier = TextMapCarrier::new()
  CompositePropagator::inject(dynamic_composite, span_ctx, new_carrier)
  
  let removed_header = TextMapCarrier::get(new_carrier, "x-dynamic-trace")
  assert_true(removed_header.length == 0, "Removed propagator should not inject headers")
}

test "复合传播器的上下文隔离" {
  // 测试不同上下文之间的隔离
  let composite = CompositePropagator::isolated([
    W3CTraceContextPropagator::new(),
    W3CBaggagePropagator::new()
  ])
  
  // 创建多个独立的上下文
  let ctx1 = SpanContext::new("isolation-trace-1", "isolation-span-1", true, "ctx1=true")
  let ctx2 = SpanContext::new("isolation-trace-2", "isolation-span-2", true, "ctx2=true")
  
  let baggage1 = Baggage::new()
  let baggage2 = Baggage::new()
  
  Baggage::set_entry(baggage1, "ctx1.key", "ctx1.value")
  Baggage::set_entry(baggage2, "ctx2.key", "ctx2.value")
  
  let carrier1 = TextMapCarrier::new()
  let carrier2 = TextMapCarrier::new()
  
  // 分别注入不同上下文
  CompositePropagator::inject_isolated(composite, ctx1, baggage1, carrier1)
  CompositePropagator::inject_isolated(composite, ctx2, baggage2, carrier2)
  
  // 提取并验证隔离
  let extracted1 = CompositePropagator::extract_isolated(composite, carrier1)
  let extracted2 = CompositePropagator::extract_isolated(composite, carrier2)
  
  assert_eq(SpanContext::trace_id(extracted1.span_ctx), "isolation-trace-1")
  assert_eq(SpanContext::trace_id(extracted2.span_ctx), "isolation-trace-2")
  assert_eq(Baggage::get_entry(extracted1.baggage, "ctx1.key"), Some("ctx1.value"))
  assert_eq(Baggage::get_entry(extracted2.baggage, "ctx2.key"), Some("ctx2.value"))
  
  // 验证交叉污染不存在
  assert_eq(Baggage::get_entry(extracted1.baggage, "ctx2.key"), None)
  assert_eq(Baggage::get_entry(extracted2.baggage, "ctx1.key"), None)
}