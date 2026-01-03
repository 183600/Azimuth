// 分布式追踪和跨服务传播综合测试
// 测试Azimuth遥测系统的分布式追踪和跨服务传播功能

test "分布式追踪链创建和管理" {
  // 测试分布式追踪链的创建和管理
  let trace_chain = TraceChain::new("user.request.12345")
  
  // 创建根span
  let root_span = TraceChain::create_root_span(trace_chain, "api.gateway")
  Span::set_attribute(root_span, "service.name", "api-gateway")
  Span::set_attribute(root_span, "http.method", "POST")
  Span::set_attribute(root_span, "http.url", "/api/orders")
  Span::set_attribute(root_span, "user.id", "user-789")
  
  // 创建子span
  let auth_span = TraceChain::create_child_span(trace_chain, root_span, "auth.service")
  Span::set_attribute(auth_span, "service.name", "auth-service")
  Span::set_attribute(auth_span, "auth.method", "jwt")
  Span::set_attribute(auth_span, "auth.result", "success")
  
  let order_span = TraceChain::create_child_span(trace_chain, root_span, "order.service")
  Span::set_attribute(order_span, "service.name", "order-service")
  Span::set_attribute(order_span, "operation.type", "create_order")
  
  // 创建嵌套子span
  let inventory_span = TraceChain::create_child_span(trace_chain, order_span, "inventory.service")
  Span::set_attribute(inventory_span, "service.name", "inventory-service")
  Span::set_attribute(inventory_span, "operation.type", "check_availability")
  Span::set_attribute(inventory_span, "product.id", "prod-123")
  
  let payment_span = TraceChain::create_child_span(trace_chain, order_span, "payment.service")
  Span::set_attribute(payment_span, "service.name", "payment-service")
  Span::set_attribute(payment_span, "operation.type", "process_payment")
  Span::set_attribute(payment_span, "payment.amount", "99.99")
  Span::set_attribute(payment_span, "payment.currency", "USD")
  
  // 结束所有span
  Span::end(auth_span)
  Span::end(inventory_span)
  Span::end(payment_span)
  Span::end(order_span)
  Span::end(root_span)
  
  // 验证追踪链
  assert_eq(TraceChain::get_trace_id(trace_chain), "user.request.12345")
  assert_eq(TraceChain::get_span_count(trace_chain), 5)
  assert_eq(TraceChain::get_root_span_name(trace_chain), "api.gateway")
  
  // 验证span层次结构
  let root_children = TraceChain::get_child_spans(trace_chain, root_span)
  assert_eq(Array::length(root_children), 2)
  
  let order_children = TraceChain::get_child_spans(trace_chain, order_span)
  assert_eq(Array::length(order_children), 2)
}

test "跨服务上下文传播" {
  // 测试跨服务上下文传播
  let context_propagator = ContextPropagator::new()
  
  // 创建原始上下文
  let original_context = Context::root()
  let trace_id = TraceId::new("trace.12345")
  let span_id = SpanId::new("span.67890")
  
  let span_context = SpanContext::new(trace_id, span_id, true, true)
  let context_with_span = Context::with_span(original_context, span_context)
  
  // 添加 baggage
  let baggage_context = Context::with_baggage(context_with_span, "user.id", "user-123")
  let baggage_context = Context::with_baggage(baggage_context, "request.id", "req-456")
  let baggage_context = Context::with_baggage(baggage_context, "session.id", "session-789")
  
  // 注入上下文到载体
  let carrier = HttpCarrier::new()
  ContextPropagator::inject(context_propagator, baggage_context, carrier)
  
  // 验证注入的头部
  let trace_header = HttpCarrier::get_header(carrier, "traceparent")
  let baggage_header = HttpCarrier::get_header(carrier, "baggage")
  
  assert_true(String::contains(trace_header.unwrap(), "trace.12345"))
  assert_true(String::contains(baggage_header.unwrap(), "user.id=user-123"))
  assert_true(String::contains(baggage_header.unwrap(), "request.id=req-456"))
  assert_true(String::contains(baggage_header.unwrap(), "session.id=session-789"))
  
  // 从载体提取上下文
  let extracted_context = ContextPropagator::extract(context_propagator, carrier)
  
  // 验证提取的上下文
  let extracted_span_context = Context::get_span(extracted_context)
  assert_true(Option::is_some(extracted_span_context))
  
  let extracted_baggage = Context::get_baggage(extracted_context, "user.id")
  assert_eq(extracted_baggage, Some("user-123"))
  
  let extracted_request_id = Context::get_baggage(extracted_context, "request.id")
  assert_eq(extracted_request_id, Some("req-456"))
  
  let extracted_session_id = Context::get_baggage(extracted_context, "session.id")
  assert_eq(extracted_session_id, Some("session-789"))
}

test "分布式追踪采样策略" {
  // 测试分布式追踪采样策略
  let sampling_config = SamplingConfig::new()
  
  // 设置基于概率的采样
  SamplingConfig::set_probabilistic_sampling(sampling_config, 0.1) // 10%采样率
  
  let probabilistic_sampler = ProbabilisticSampler::new(0.1)
  
  // 测试采样决策
  let mut sampled_count = 0
  for i in 0..1000 {
    let trace_id = TraceId::random()
    let decision = ProbabilisticSampler::should_sample(probabilistic_sampler, trace_id)
    
    if decision {
      sampled_count = sampled_count + 1
    }
  }
  
  // 验证采样率大约为10%
  let sampling_rate = (sampled_count.to_float() / 1000.0) * 100.0
  assert_true(sampling_rate > 5.0 && sampling_rate < 15.0) // 允许一定的偏差
  
  // 设置基于属性的采样
  SamplingConfig::set_attribute_based_sampling(sampling_config, [
    ("service.name", "critical-service"),
    ("http.status_code", "5xx"),
    ("error.type", "*")
  ])
  
  let attribute_sampler = AttributeBasedSampler::new([
    ("service.name", "critical-service"),
    ("http.status_code", "5xx"),
    ("error.type", "*")
  ])
  
  // 测试基于属性的采样
  let critical_context = Context::with_attribute(Context::root(), "service.name", "critical-service")
  let error_context = Context::with_attribute(Context::root(), "http.status_code", "500")
  let normal_context = Context::with_attribute(Context::root(), "service.name", "normal-service")
  
  assert_true(AttributeBasedSampler::should_sample(attribute_sampler, critical_context))
  assert_true(AttributeBasedSampler::should_sample(attribute_sampler, error_context))
  assert_false(AttributeBasedSampler::should_sample(attribute_sampler, normal_context))
  
  // 设置复合采样策略
  let composite_sampler = CompositeSampler::new()
  CompositeSampler::add_sampler(composite_sampler, probabilistic_sampler)
  CompositeSampler::add_sampler(composite_sampler, attribute_sampler)
  
  // 测试复合采样
  assert_true(CompositeSampler::should_sample(composite_sampler, critical_context)) // 属性采样优先
}

test "跨服务追踪一致性" {
  // 测试跨服务追踪一致性
  let consistency_checker = ConsistencyChecker::new()
  
  // 模拟多个服务的追踪数据
  let service_a_spans = []
  let service_b_spans = []
  let service_c_spans = []
  
  // 服务A的追踪数据
  let a_root_span = SpanData::new("service.a.operation", "trace.consistency.1")
  SpanData::add_attribute(a_root_span, "service.name", "service-a")
  SpanData::add_attribute(a_root_span, "service.version", "1.0.0")
  SpanData::add_event(a_root_span, "operation.started", 1704067200000L)
  SpanData::add_event(a_root_span, "operation.completed", 1704067202000L)
  Array::push(service_a_spans, a_root_span)
  
  // 服务B的追踪数据
  let b_child_span = SpanData::new("service.b.operation", "trace.consistency.1")
  SpanData::add_attribute(b_child_span, "service.name", "service-b")
  SpanData::add_attribute(b_child_span, "service.version", "2.1.0")
  SpanData::add_attribute(b_child_span, "parent.span", a_root_span.span_id)
  SpanData::add_event(b_child_span, "operation.started", 1704067201000L)
  SpanData::add_event(b_child_span, "operation.completed", 1704067201500L)
  Array::push(service_b_spans, b_child_span)
  
  // 服务C的追踪数据
  let c_child_span = SpanData::new("service.c.operation", "trace.consistency.1")
  SpanData::add_attribute(c_child_span, "service.name", "service-c")
  SpanData::add_attribute(c_child_span, "service.version", "1.5.2")
  SpanData::add_attribute(c_child_span, "parent.span", b_child_span.span_id)
  SpanData::add_event(c_child_span, "operation.started", 1704067201200L)
  SpanData::add_event(c_child_span, "operation.completed", 1704067201400L)
  Array::push(service_c_spans, c_child_span)
  
  // 检查一致性
  let consistency_report = ConsistencyChecker::check_trace_consistency(
    consistency_checker, 
    service_a_spans, 
    service_b_spans, 
    service_c_spans
  )
  
  // 验证一致性报告
  assert_true(ConsistencyReport::is_trace_id_consistent(consistency_report))
  assert_true(ConsistencyReport::is_parent_child_relationship_consistent(consistency_report))
  assert_true(ConsistencyReport::is_timeline_consistent(consistency_report))
  
  // 创建不一致的数据进行测试
  let inconsistent_span = SpanData::new("service.d.operation", "trace.inconsistent.2")
  SpanData::add_attribute(inconsistent_span, "service.name", "service-d")
  SpanData::add_attribute(inconsistent_span, "parent.span", "non.existent.span")
  Array::push(service_c_spans, inconsistent_span)
  
  // 检查不一致性
  let inconsistency_report = ConsistencyChecker::check_trace_consistency(
    consistency_checker, 
    service_a_spans, 
    service_b_spans, 
    service_c_spans
  )
  
  // 验证检测到不一致性
  assert_false(ConsistencyReport::is_parent_child_relationship_consistent(inconsistency_report))
  assert_true(ConsistencyReport::get_inconsistency_count(inconsistency_report) > 0)
}

test "分布式追踪性能优化" {
  // 测试分布式追踪性能优化
  let performance_optimizer = PerformanceOptimizer::new()
  
  // 设置性能优化配置
  PerformanceOptimizer::set_max_spans_per_trace(performance_optimizer, 1000)
  PerformanceOptimizer::set_max_attributes_per_span(performance_optimizer, 128)
  PerformanceOptimizer::set_max_events_per_span(performance_optimizer, 100)
  PerformanceOptimizer::set_max_links_per_span(performance_optimizer, 10)
  
  // 创建大量span测试性能
  let trace_id = TraceId::random()
  let mut span_count = 0
  
  for i in 0..1500 {
    let span_data = SpanData::new("performance.test." + i.to_string(), trace_id)
    
    // 添加大量属性
    for j in 0..150 {
      SpanData::add_attribute(span_data, "attr." + j.to_string(), "value." + j.to_string())
    }
    
    // 添加大量事件
    for k in 0..150 {
      SpanData::add_event(span_data, "event." + k.to_string(), 1704067200000L + k.to_long())
    }
    
    // 应用性能优化
    let optimized_span = PerformanceOptimizer::optimize_span(performance_optimizer, span_data)
    
    // 验证优化效果
    let optimized_attrs = SpanData::get_attributes(optimized_span)
    let optimized_events = SpanData::get_events(optimized_span)
    
    assert_true(AttributeMap::size(optimized_attrs) <= 128)
    assert_true(Array::length(optimized_events) <= 100)
    
    span_count = span_count + 1
    
    // 测试span数量限制
    if span_count >= 1000 {
      break
    }
  }
  
  // 测试批量处理优化
  let batch_spans = []
  for i in 0..500 {
    let span_data = SpanData::new("batch.test." + i.to_string(), trace_id)
    Array::push(batch_spans, span_data)
  }
  
  let optimized_batch = PerformanceOptimizer::optimize_batch(performance_optimizer, batch_spans)
  
  // 验证批量优化
  assert_true(Array::length(optimized_batch) <= 1000)
}

test "分布式追踪安全性和隐私" {
  // 测试分布式追踪安全性和隐私
  let security_manager = SecurityManager::new()
  
  // 设置敏感数据规则
  SecurityManager::add_sensitive_attribute(security_manager, "user.email")
  SecurityManager::add_sensitive_attribute(security_manager, "user.phone")
  SecurityManager::add_sensitive_attribute(security_manager, "credit.card.number")
  SecurityManager::add_sensitive_attribute(security_manager, "social.security.number")
  SecurityManager::add_sensitive_attribute(security_manager, "password")
  
  // 创建包含敏感数据的span
  let sensitive_span = SpanData::new("secure.operation", "trace.secure.1")
  SpanData::add_attribute(sensitive_span, "user.email", "user@example.com")
  SpanData::add_attribute(sensitive_span, "user.phone", "+1234567890")
  SpanData::add_attribute(sensitive_span, "credit.card.number", "4111111111111111")
  SpanData::add_attribute(sensitive_span, "user.name", "John Doe")
  SpanData::add_attribute(sensitive_span, "user.id", "user-123")
  
  // 应用安全过滤
  let filtered_span = SecurityManager::filter_sensitive_data(security_manager, sensitive_span)
  
  // 验证敏感数据已被过滤
  let filtered_attrs = SpanData::get_attributes(filtered_span)
  
  assert_false(AttributeMap::contains(filtered_attrs, "user.email"))
  assert_false(AttributeMap::contains(filtered_attrs, "user.phone"))
  assert_false(AttributeMap::contains(filtered_attrs, "credit.card.number"))
  assert_true(AttributeMap::contains(filtered_attrs, "user.name"))
  assert_true(AttributeMap::contains(filtered_attrs, "user.id"))
  
  // 测试数据脱敏
  let masked_span = SecurityManager::mask_sensitive_data(security_manager, sensitive_span)
  let masked_attrs = SpanData::get_attributes(masked_span)
  
  // 验证脱敏效果
  let masked_email = AttributeMap::get(masked_attrs, "user.email")
  let masked_phone = AttributeMap::get(masked_attrs, "user.phone")
  let masked_credit_card = AttributeMap::get(masked_attrs, "credit.card.number")
  
  assert_eq(masked_email, Some("u***@example.com"))
  assert_eq(masked_phone, Some("+1******90"))
  assert_eq(masked_credit_card, Some("4111********1111"))
  
  // 测试加密
  let encryption_key = EncryptionKey::generate()
  let encrypted_span = SecurityManager::encrypt_sensitive_data(security_manager, sensitive_span, encryption_key)
  let decrypted_span = SecurityManager::decrypt_sensitive_data(security_manager, encrypted_span, encryption_key)
  
  // 验证加密/解密
  let decrypted_attrs = SpanData::get_attributes(decrypted_span)
  assert_eq(AttributeMap::get(decrypted_attrs, "user.email"), Some("user@example.com"))
  assert_eq(AttributeMap::get(decrypted_attrs, "user.phone"), Some("+1234567890"))
}

test "分布式追踪故障恢复" {
  // 测试分布式追踪故障恢复
  let fault_recovery = FaultRecovery::new()
  
  // 设置故障恢复策略
  FaultRecovery::set_retry_policy(fault_recovery, ExponentialBackoff, 3, 1000)
  FaultRecovery::set_circuit_breaker_threshold(fault_recovery, 5)
  FaultRecovery::set_timeout(fault_recovery, 5000)
  
  // 模拟追踪数据发送失败
  let trace_data = TraceData::new("trace.fault.test")
  let span_data = SpanData::new("fault.test.operation", "trace.fault.test")
  TraceData::add_span(trace_data, span_data)
  
  // 设置故障注入
  let fault_injector = FaultInjector::new()
  FaultInjector::set_failure_rate(fault_injector, 0.5) // 50%失败率
  FaultInjector::set_failure_type(fault_injector, NetworkError)
  
  // 测试故障恢复
  let mut success_count = 0
  let mut failure_count = 0
  
  for i in 0..10 {
    let result = FaultRecovery::send_with_retry(fault_recovery, trace_data, fault_injector)
    
    match result {
      Success => success_count = success_count + 1
      Failure => failure_count = failure_count + 1
    }
  }
  
  // 验证故障恢复效果
  assert_true(success_count > 0) // 至少有一些成功
  assert_true(success_count + failure_count == 10)
  
  // 测试断路器
  for i in 0..10 {
    FaultRecovery::send_with_retry(fault_recovery, trace_data, fault_injector)
  }
  
  // 验证断路器触发
  assert_true(FaultRecovery::is_circuit_breaker_open(fault_recovery))
  
  // 测试断路器恢复
  FaultInjector::set_failure_rate(fault_injector, 0.0) // 恢复正常
  
  // 等待断路器半开状态
  FaultRecovery::wait_for_circuit_breaker_half_open(fault_recovery)
  
  // 测试断路器关闭
  let recovery_result = FaultRecovery::send_with_retry(fault_recovery, trace_data, fault_injector)
  assert_true(Result::is_success(recovery_result))
  assert_false(FaultRecovery::is_circuit_breaker_open(fault_recovery))
}