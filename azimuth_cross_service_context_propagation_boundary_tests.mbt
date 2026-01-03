// 跨服务上下文传播的边界情况测试
// 测试跨服务传播中的各种边界情况和异常场景

test "跨服务传播中的超长上下文处理" {
  // 测试超长上下文的传播处理
  let trace_id = "trace-" + "a" * 100  // 创建超长trace ID
  let span_id = "span-" + "b" * 50     // 创建超长span ID
  let span_ctx = SpanContext::new(trace_id, span_id, true, "key=" + "c" * 1000)
  
  let carrier = TextMapCarrier::new()
  let propagator = W3CTraceContextPropagator::new()
  
  // 注入超长上下文
  W3CTraceContextPropagator::inject(propagator, span_ctx, carrier)
  
  // 验证超长上下文被正确处理（截断或分段）
  let traceparent_header = TextMapCarrier::get(carrier, "traceparent")
  assert_true(traceparent_header.length > 0, "Should have traceparent header even with long context")
  
  // 提取并验证
  let extracted_ctx = W3CTraceContextPropagator::extract(propagator, carrier)
  assert_true(SpanContext::is_valid(extracted_ctx), "Extracted context should be valid")
  
  // 验证截断处理
  let extracted_trace_id = SpanContext::trace_id(extracted_ctx)
  let extracted_span_id = SpanContext::span_id(extracted_ctx)
  assert_true(extracted_trace_id.length <= 128, "Trace ID should be truncated to reasonable length")
  assert_true(extracted_span_id.length <= 64, "Span ID should be truncated to reasonable length")
}

test "跨服务传播中的特殊字符和编码处理" {
  // 测试包含特殊字符的上下文传播
  let trace_id = "trace-special-!@#$%^&*()_+-=[]{}|;':\",./<>?"
  let span_id = "span-unicode-测试-🚀-📊-📈"
  let trace_state = "key1=value1 with spaces;key2=value2@#$%;key3=测试值"
  
  let span_ctx = SpanContext::new(trace_id, span_id, true, trace_state)
  let carrier = TextMapCarrier::new()
  let propagator = W3CTraceContextPropagator::new()
  
  // 注入包含特殊字符的上下文
  W3CTraceContextPropagator::inject(propagator, span_ctx, carrier)
  
  // 提取并验证
  let extracted_ctx = W3CTraceContextPropagator::extract(propagator, carrier)
  
  // 验证特殊字符处理
  assert_true(SpanContext::is_valid(extracted_ctx), "Context with special characters should be valid")
  assert_true(SpanContext::trace_id(extracted_ctx).contains("trace-special"), "Should preserve readable part")
}

test "跨服务传播中的循环依赖检测" {
  // 测试服务调用链中的循环依赖检测
  let service_a_ctx = SpanContext::new("trace-cycle", "span-a", true, "")
  let carrier = TextMapCarrier::new()
  let propagator = W3CTraceContextPropagator::new()
  
  // 服务A注入上下文
  W3CTraceContextPropagator::inject(propagator, service_a_ctx, carrier)
  
  // 服务B提取并创建子span
  let service_b_ctx = W3CTraceContextPropagator::extract(propagator, carrier)
  let service_b_span = Span::new("service-b-operation", Server, service_b_ctx)
  
  // 模拟服务B调用回服务A
  let return_carrier = TextMapCarrier::new()
  W3CTraceContextPropagator::inject(propagator, Span::span_context(service_b_span), return_carrier)
  
  // 服务A检测循环依赖
  let return_ctx = W3CTraceContextPropagator::extract(propagator, return_carrier)
  let cycle_detector = Context::CycleDetector::new()
  let is_circular = Context::CycleDetector::detect(cycle_detector, service_a_ctx, return_ctx)
  
  assert_true(is_circular, "Should detect circular dependency in service calls")
}

test "跨服务传播中的上下文损坏恢复" {
  // 测试上下文损坏时的恢复机制
  let original_ctx = SpanContext::new("trace-corruption", "span-original", true, "key=value")
  let carrier = TextMapCarrier::new()
  let propagator = W3CTraceContextPropagator::new()
  
  // 正常注入
  W3CTraceContextPropagator::inject(propagator, original_ctx, carrier)
  
  // 模拟上下文损坏
  TextMapCarrier::set(carrier, "traceparent", "corrupted-header-value")
  TextMapCarrier::set(carrier, "extra-header", "malicious-content")
  
  // 尝试提取并恢复
  let recovery_propagator = W3CTraceContextPropagator::with_recovery()
  let recovered_ctx = W3CTraceContextPropagator::extract(recovery_propagator, carrier)
  
  // 验证恢复机制
  assert_true(SpanContext::is_valid(recovered_ctx), "Should recover from corrupted context")
  assert_not_eq(SpanContext::trace_id(recovered_ctx), "", "Should have valid trace ID after recovery")
}

test "跨服务传播中的并发上下文管理" {
  // 测试并发场景下的上下文传播
  let base_ctx = SpanContext::new("trace-concurrent", "span-base", true, "")
  let propagator = W3CTraceContextPropagator::new()
  
  // 创建多个并发传播任务
  let concurrent_tasks = []
  for i in 0..10 {
    let task = fn() {
      let carrier = TextMapCarrier::new()
      W3CTraceContextPropagator::inject(propagator, base_ctx, carrier)
      
      // 模拟网络延迟
      // Thread::sleep(10)
      
      let extracted_ctx = W3CTraceContextPropagator::extract(propagator, carrier)
      return extracted_ctx
    }
    concurrent_tasks.push(task)
  }
  
  // 执行并发任务
  let results = []
  for task in concurrent_tasks {
    let result = task()
    results.push(result)
  }
  
  // 验证所有结果一致
  for i in 1..results.length {
    assert_eq(SpanContext::trace_id(results[0]), SpanContext::trace_id(results[i]), "All contexts should have same trace ID")
    assert_true(SpanContext::is_valid(results[i]), "All extracted contexts should be valid")
  }
}

test "跨服务传播中的baggage大小限制" {
  // 测试baggage大小限制处理
  let baggage = Baggage::new()
  let large_value = "x" * 10000  // 创建超大值
  
  // 添加大量baggage条目
  for i in 0..100 {
    let key = "baggage.key." + i.to_string()
    let value = if i == 50 { large_value } else { "value-" + i.to_string() }
    Baggage::set_entry(baggage, key, value)
  }
  
  let carrier = TextMapCarrier::new()
  let baggage_propagator = W3CBaggagePropagator::with_limits(8192) // 8KB限制
  
  // 注入baggage
  W3CBaggagePropagator::inject(baggage_propagator, baggage, carrier)
  
  // 验证大小限制处理
  let baggage_header = TextMapCarrier::get(carrier, "baggage")
  assert_true(baggage_header.length > 0, "Should have baggage header")
  assert_true(baggage_header.length <= 8192, "Baggage header should respect size limit")
  
  // 提取并验证
  let extracted_baggage = W3CBaggagePropagator::extract(baggage_propagator, carrier)
  
  // 验证重要条目保留
  let important_entry = Baggage::get_entry(extracted_baggage, "baggage.key.0")
  assert_eq(important_entry, Some("value-0"), "Important entries should be preserved")
}

test "跨服务传播中的上下文版本兼容性" {
  // 测试不同版本间的上下文兼容性
  let v1_ctx = SpanContext::new("trace-version", "span-v1", true, "")
  let carrier = TextMapCarrier::new()
  let v1_propagator = W3CTraceContextPropagator::with_version("v1")
  
  // 使用v1格式注入
  W3CTraceContextPropagator::inject(v1_propagator, v1_ctx, carrier)
  
  // 使用v2格式提取
  let v2_propagator = W3CTraceContextPropagator::with_version("v2")
  let extracted_ctx = W3CTraceContextPropagator::extract(v2_propagator, carrier)
  
  // 验证向后兼容性
  assert_true(SpanContext::is_valid(extracted_ctx), "v2 should be able to extract v1 context")
  assert_eq(SpanContext::trace_id(extracted_ctx), "trace-version", "Trace ID should be preserved across versions")
  
  // 测试向前兼容性
  let v2_ctx = SpanContext::new("trace-version-v2", "span-v2", true, "")
  let v2_carrier = TextMapCarrier::new()
  W3CTraceContextPropagator::inject(v2_propagator, v2_ctx, v2_carrier)
  
  let v1_extracted_ctx = W3CTraceContextPropagator::extract(v1_propagator, v2_carrier)
  assert_true(SpanContext::is_valid(v1_extracted_ctx), "v1 should handle v2 context gracefully")
}

test "跨服务传播中的网络分区恢复" {
  // 测试网络分区情况下的上下文恢复
  let primary_ctx = SpanContext::new("trace-partition", "span-primary", true, "primary=true")
  let backup_ctx = SpanContext::new("trace-partition-backup", "span-backup", true, "backup=true")
  
  let carrier = TextMapCarrier::new()
  let propagator = W3CTraceContextPropagator::with_fallback()
  
  // 正常情况注入主要上下文
  W3CTraceContextPropagator::inject(propagator, primary_ctx, carrier)
  
  // 模拟网络分区，主要上下文丢失
  TextMapCarrier::remove(carrier, "traceparent")
  
  // 使用备用上下文
  W3CTraceContextPropagator::inject_fallback(propagator, backup_ctx, carrier)
  
  // 提取并验证恢复
  let recovered_ctx = W3CTraceContextPropagator::extract_with_fallback(propagator, carrier)
  
  assert_true(SpanContext::is_valid(recovered_ctx), "Should recover from network partition")
  assert_eq(SpanContext::trace_id(recovered_ctx), "trace-partition-backup", "Should use fallback context")
}

test "跨服务传播中的上下文安全过滤" {
  // 测试敏感信息的安全过滤
  let sensitive_ctx = SpanContext::new("trace-secure", "span-secure", true, 
    "user.id=12345;session.token=secret123;public.info=safe")
  
  let carrier = TextMapCarrier::new()
  let secure_propagator = W3CTraceContextPropagator::with_security_filter()
  
  // 添加安全过滤规则
  W3CTraceContextPropagator::add_filter_rule(secure_propagator, "session.token", "redact")
  W3CTraceContextPropagator::add_filter_rule(secure_propagator, "user.id", "hash")
  
  // 注入带安全过滤的上下文
  W3CTraceContextPropagator::inject(secure_propagator, sensitive_ctx, carrier)
  
  // 提取并验证过滤结果
  let filtered_ctx = W3CTraceContextPropagator::extract(secure_propagator, carrier)
  let filtered_state = SpanContext::trace_state(filtered_ctx)
  
  // 验证敏感信息被过滤
  assert_true(filtered_state.contains("public.info=safe"), "Public info should be preserved")
  assert_true(filtered_state.contains("session.token=[REDACTED]"), "Sensitive token should be redacted")
  assert_true(filtered_state.contains("user.id=[HASHED]"), "User ID should be hashed")
  assert_false(filtered_state.contains("secret123"), "Original secret should not be present")
}