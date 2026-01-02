// Azimuth 分布式追踪一致性增强测试用例
// 专注于分布式系统中追踪数据的一致性和完整性

// 测试1: 跨服务追踪上下文传播一致性
test "跨服务追踪上下文传播一致性测试" {
  let tracer_provider = TracerProvider::new()
  let tracer = TracerProvider::get_tracer(tracer_provider, "consistency.test")
  
  // 创建根追踪上下文
  let root_span = Tracer::start_span(tracer, "user.request")
  let root_context = Span::context(root_span)
  
  // 验证根上下文完整性
  assert_true(TraceContext::is_valid(root_context))
  assert_eq(TraceContext::trace_id(root_context).length(), 32)
  assert_eq(TraceContext::span_id(root_context).length(), 16)
  
  // 模拟服务间传播
  let service_a_context = TraceContext::extract_from_headers([
    ("traceparent", TraceContext::to_string(root_context)),
    ("x-trace-id", TraceContext::trace_id(root_context))
  ])
  
  let service_b_context = TraceContext::extract_from_headers([
    ("traceparent", TraceContext::to_string(service_a_context)),
    ("x-request-id", "req-12345")
  ])
  
  // 验证追踪ID一致性
  assert_eq(TraceContext::trace_id(root_context), TraceContext::trace_id(service_a_context))
  assert_eq(TraceContext::trace_id(root_context), TraceContext::trace_id(service_b_context))
  
  // 验证span ID唯一性
  assert_not_eq(TraceContext::span_id(root_context), TraceContext::span_id(service_a_context))
  assert_not_eq(TraceContext::span_id(service_a_context), TraceContext::span_id(service_b_context))
  
  Span::end(root_span)
}

// 测试2: 分布式追踪采样一致性
test "分布式追踪采样一致性测试" {
  let sampler_config = SamplerConfig::new_probabilistic(0.1) // 10%采样率
  let sampler = Sampler::from_config(sampler_config)
  
  let sampled_spans = Array::empty()
  let unsampled_spans = Array::empty()
  
  // 生成大量追踪请求
  for i = 0; i < 1000; i = i + 1 {
    let trace_id = TraceId::generate()
    let sampling_decision = Sampler::should_sample(sampler, trace_id)
    
    if sampling_decision {
      Array::push(sampled_spans, trace_id)
    } else {
      Array::push(unsampled_spans, trace_id)
    }
  }
  
  // 验证采样率的统计一致性
  let sampled_ratio = sampled_spans.length() / 1000.0
  assert_true(sampled_ratio > 0.05 && sampled_ratio < 0.15) // 允许5%误差
  
  // 验证采样决策的一致性
  let test_trace_id = TraceId::from_string("1234567890abcdef1234567890abcdef")
  let decision1 = Sampler::should_sample(sampler, test_trace_id)
  let decision2 = Sampler::should_sample(sampler, test_trace_id)
  assert_eq(decision1, decision2) // 相同trace ID应该有相同的采样决策
}

// 测试3: 分布式追踪时间戳一致性
test "分布式追踪时间戳一致性测试" {
  let time_sync = TimeSynchronizer::new()
  let tracer_provider = TracerProvider::with_time_sync(time_sync)
  let tracer = TracerProvider::get_tracer(tracer_provider, "timestamp.consistency")
  
  // 在不同服务节点创建span
  let service_a_span = Tracer::start_span_with_time(
    tracer, 
    "service.a.operation", 
    TimeSynchronizer::get_synced_time(time_sync, "node-a")
  )
  
  let service_b_span = Tracer::start_span_with_time(
    tracer, 
    "service.b.operation", 
    TimeSynchronizer::get_synced_time(time_sync, "node-b")
  )
  
  // 验证时间戳顺序逻辑
  let service_a_start = Span::start_time(service_a_span)
  let service_b_start = Span::start_time(service_b_span)
  
  // 模拟服务依赖关系：A调用B
  Span::add_event(service_a_span, "calling.service.b", Some(service_b_start))
  Span::add_event(service_b_span, "called.by.service.a", Some(service_a_start))
  
  // 验证时间戳关系
  assert_true(Time::is_after_or_equal(service_b_start, service_a_start))
  
  // 验证时钟偏移在合理范围内
  let clock_offset = TimeSynchronizer::get_clock_offset(time_sync, "node-a", "node-b")
  assert_true(Time::duration_ms(clock_offset) < 100) // 偏移小于100ms
  
  Span::end(service_a_span)
  Span::end(service_b_span)
}

// 测试4: 分布式追踪数据完整性
test "分布式追踪数据完整性测试" {
  let trace_collector = TraceCollector::new()
  let integrity_checker = TraceIntegrityChecker::new()
  
  // 配置完整性检查规则
  TraceIntegrityChecker::add_rule(integrity_checker, "parent_span_exists")
  TraceIntegrityChecker::add_rule(integrity_checker, "timestamp_sequence_valid")
  TraceIntegrityChecker::add_rule(integrity_checker, "no_orphan_spans")
  
  // 创建复杂的追踪树结构
  let root_span = TraceCollector::create_span(trace_collector, "root.operation")
  let child_span1 = TraceCollector::create_child_span(trace_collector, root_span, "child.operation.1")
  let child_span2 = TraceCollector::create_child_span(trace_collector, root_span, "child.operation.2")
  let grandchild_span = TraceCollector::create_child_span(trace_collector, child_span1, "grandchild.operation")
  
  // 设置span关系
  Span::set_parent(root_span, None)
  Span::set_parent(child_span1, Some(root_span))
  Span::set_parent(child_span2, Some(root_span))
  Span::set_parent(grandchild_span, Some(child_span1))
  
  // 完成所有span
  Span::end(grandchild_span)
  Span::end(child_span2)
  Span::end(child_span1)
  Span::end(root_span)
  
  // 收集追踪数据
  let trace_data = TraceCollector::collect(trace_collector)
  
  // 执行完整性检查
  let integrity_result = TraceIntegrityChecker::validate(integrity_checker, trace_data)
  
  // 验证检查结果
  assert_true(IntegrityResult::is_passed(integrity_result))
  assert_eq(IntegrityResult::span_count(integrity_result), 4)
  assert_eq(IntegrityResult::tree_depth(integrity_result), 3)
  assert_eq(IntegrityResult::orphan_count(integrity_result), 0)
}