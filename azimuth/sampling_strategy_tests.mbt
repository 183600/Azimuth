// 采样策略测试
// 测试遥测系统中不同的采样策略和算法

import "azimuth/azimuth"

pub test "基础采样策略测试" {
  // 创建不同类型的采样器
  let always_on_sampler = azimuth::AlwaysOnSampler::new()
  let always_off_sampler = azimuth::AlwaysOffSampler::new()
  
  let trace_id = "trace-12345"
  let span_name = "test-span"
  let span_kind = azimuth::Internal
  let parent_context = azimuth::SpanContext::new(trace_id, "parent-span", true, "")
  
  // 测试AlwaysOn采样器
  let sampling_decision1 = azimuth::Sampler::should_sample(
    always_on_sampler,
    parent_context,
    trace_id,
    span_name,
    span_kind,
    []
  )
  
  assert_true(azimuth::SamplingDecision::is_sampled(sampling_decision1))
  assert_eq(azimuth::SamplingDecision::decision(sampling_decision1), azimuth::RecordAndSample)
  
  // 测试AlwaysOff采样器
  let sampling_decision2 = azimuth::Sampler::should_sample(
    always_off_sampler,
    parent_context,
    trace_id,
    span_name,
    span_kind,
    []
  )
  
  assert_false(azimuth::SamplingDecision::is_sampled(sampling_decision2))
  assert_eq(azimuth::SamplingDecision::decision(sampling_decision2), azimuth::Drop)
}

pub test "概率采样测试" {
  // 创建不同概率的采样器
  let high_probability_sampler = azimuth::TraceIdRatioBasedSampler::new(0.8)  // 80%采样率
  let medium_probability_sampler = azimuth::TraceIdRatioBasedSampler::new(0.5)  // 50%采样率
  let low_probability_sampler = azimuth::TraceIdRatioBasedSampler::new(0.1)   // 10%采样率
  
  let trace_id = "trace-12345"
  let span_name = "test-span"
  let span_kind = azimuth::Internal
  let parent_context = azimuth::SpanContext::new(trace_id, "parent-span", true, "")
  
  // 测试高概率采样器
  let high_sampled = 0
  for i in 0..100 {
    let test_trace_id = "trace-" + i.to_string()
    let sampling_decision = azimuth::Sampler::should_sample(
      high_probability_sampler,
      parent_context,
      test_trace_id,
      span_name,
      span_kind,
      []
    )
    
    if (azimuth::SamplingDecision::is_sampled(sampling_decision)) {
      high_sampled = high_sampled + 1
    }
  }
  
  // 验证采样概率（允许一定的误差）
  assert_true(high_sampled >= 70 && high_sampled <= 90)
  
  // 测试中等概率采样器
  let medium_sampled = 0
  for i in 0..100 {
    let test_trace_id = "trace-" + i.to_string()
    let sampling_decision = azimuth::Sampler::should_sample(
      medium_probability_sampler,
      parent_context,
      test_trace_id,
      span_name,
      span_kind,
      []
    )
    
    if (azimuth::SamplingDecision::is_sampled(sampling_decision)) {
      medium_sampled = medium_sampled + 1
    }
  }
  
  assert_true(medium_sampled >= 40 && medium_sampled <= 60)
  
  // 测试低概率采样器
  let low_sampled = 0
  for i in 0..100 {
    let test_trace_id = "trace-" + i.to_string()
    let sampling_decision = azimuth::Sampler::should_sample(
      low_probability_sampler,
      parent_context,
      test_trace_id,
      span_name,
      span_kind,
      []
    )
    
    if (azimuth::SamplingDecision::is_sampled(sampling_decision)) {
      low_sampled = low_sampled + 1
    }
  }
  
  assert_true(low_sampled >= 5 && low_sampled <= 15)
}

pub test "基于属性的采样测试" {
  // 创建基于属性的采样器
  let attribute_rules = [
    ("service.name", "critical-service", true),
    ("http.status_code", "500", true),
    ("error.type", "critical", true),
    ("user.premium", "true", true)
  ]
  
  let attribute_based_sampler = azimuth::AttributeBasedSampler::new(attribute_rules)
  
  let trace_id = "trace-12345"
  let span_name = "test-span"
  let span_kind = azimuth::Internal
  let parent_context = azimuth::SpanContext::new(trace_id, "parent-span", true, "")
  
  // 测试匹配关键服务的Span
  let critical_service_attrs = [
    ("service.name", azimuth::StringValue("critical-service")),
    ("operation.name", azimuth::StringValue("api.request"))
  ]
  
  let sampling_decision1 = azimuth::Sampler::should_sample(
    attribute_based_sampler,
    parent_context,
    trace_id,
    span_name,
    span_kind,
    critical_service_attrs
  )
  
  assert_true(azimuth::SamplingDecision::is_sampled(sampling_decision1))
  
  // 测试匹配错误状态的Span
  let error_attrs = [
    ("service.name", azimuth::StringValue("normal-service")),
    ("http.status_code", azimuth::StringValue("500"))
  ]
  
  let sampling_decision2 = azimuth::Sampler::should_sample(
    attribute_based_sampler,
    parent_context,
    trace_id,
    span_name,
    span_kind,
    error_attrs
  )
  
  assert_true(azimuth::SamplingDecision::is_sampled(sampling_decision2))
  
  // 测试不匹配任何规则的Span
  let normal_attrs = [
    ("service.name", azimuth::StringValue("normal-service")),
    ("http.status_code", azimuth::StringValue("200"))
  ]
  
  let sampling_decision3 = azimuth::Sampler::should_sample(
    attribute_based_sampler,
    parent_context,
    trace_id,
    span_name,
    span_kind,
    normal_attrs
  )
  
  assert_false(azimuth::SamplingDecision::is_sampled(sampling_decision3))
}

pub test "父级基础采样测试" {
  // 创建父级基础采样器
  let parent_based_sampler = azimuth::ParentBasedSampler::new(
    azimuth::TraceIdRatioBasedSampler::new(0.5)  // 50%概率采样器作为根采样器
  )
  
  let trace_id = "trace-12345"
  let span_name = "test-span"
  let span_kind = azimuth::Internal
  
  // 测试有采样父级的Span
  let sampled_parent_context = azimuth::SpanContext::new(trace_id, "sampled-parent", true, "")
  let sampling_decision1 = azimuth::Sampler::should_sample(
    parent_based_sampler,
    sampled_parent_context,
    trace_id,
    span_name,
    span_kind,
    []
  )
  
  assert_true(azimuth::SamplingDecision::is_sampled(sampling_decision1))
  
  // 测试有未采样父级的Span
  let unsampled_parent_context = azimuth::SpanContext::new(trace_id, "unsampled-parent", false, "")
  let sampling_decision2 = azimuth::Sampler::should_sample(
    parent_based_sampler,
    unsampled_parent_context,
    trace_id,
    span_name,
    span_kind,
    []
  )
  
  assert_false(azimuth::SamplingDecision::is_sampled(sampling_decision2))
  
  // 测试没有父级的Span（使用根采样器）
  let no_parent_context = azimuth::SpanContext::new("", "", false, "")
  let sampled_count = 0
  
  for i in 0..100 {
    let test_trace_id = "trace-" + i.to_string()
    let sampling_decision = azimuth::Sampler::should_sample(
      parent_based_sampler,
      no_parent_context,
      test_trace_id,
      span_name,
      span_kind,
      []
    )
    
    if (azimuth::SamplingDecision::is_sampled(sampling_decision)) {
      sampled_count = sampled_count + 1
    }
  }
  
  // 验证根采样器的概率（允许误差）
  assert_true(sampled_count >= 40 && sampled_count <= 60)
}

pub test "自适应采样测试" {
  // 创建自适应采样器
  let adaptive_sampler = azimuth::AdaptiveSampler::new(
    1000,  // 目标采样数
    60     // 时间窗口（秒）
  )
  
  let trace_id = "trace-12345"
  let span_name = "test-span"
  let span_kind = azimuth::Internal
  let parent_context = azimuth::SpanContext::new(trace_id, "parent-span", true, "")
  
  // 初始阶段：低流量，应该高采样率
  let initial_sampled = 0
  for i in 0..100 {
    let test_trace_id = "trace-" + i.to_string()
    let sampling_decision = azimuth::Sampler::should_sample(
      adaptive_sampler,
      parent_context,
      test_trace_id,
      span_name,
      span_kind,
      []
    )
    
    if (azimuth::SamplingDecision::is_sampled(sampling_decision)) {
      initial_sampled = initial_sampled + 1
    }
  }
  
  // 低流量时应该有高采样率
  assert_true(initial_sampled >= 80)
  
  // 模拟高流量场景
  let high_traffic_sampled = 0
  for i in 0..2000 {
    let test_trace_id = "trace-" + i.to_string()
    let sampling_decision = azimuth::Sampler::should_sample(
      adaptive_sampler,
      parent_context,
      test_trace_id,
      span_name,
      span_kind,
      []
    )
    
    if (azimuth::SamplingDecision::is_sampled(sampling_decision)) {
      high_traffic_sampled = high_traffic_sampled + 1
    }
  }
  
  // 高流量时应该调整到目标采样数附近
  assert_true(high_traffic_sampled >= 900 && high_traffic_sampled <= 1100)
  
  // 验证自适应采样器统计
  let sampler_stats = azimuth::AdaptiveSampler::statistics(adaptive_sampler)
  assert_eq(azimuth::SamplerStats::total_spans(sampler_stats), 2100)
  assert_true(azimuth::SamplerStats::sampled_spans(sampler_stats) >= 900)
  assert_true(azimuth::SamplerStats::sampled_spans(sampler_stats) <= 1100)
}

pub test "复合采样策略测试" {
  // 创建多个采样器
  let priority_sampler = azimuth::PriorityBasedSampler::new()
  azimuth::PriorityBasedSampler::add_sampler(priority_sampler, 
    azimuth::AttributeBasedSampler::new([("error.type", "critical", true)]), 
    100  // 最高优先级
  )
  azimuth::PriorityBasedSampler::add_sampler(priority_sampler, 
    azimuth::TraceIdRatioBasedSampler::new(0.8), 
    50   // 中等优先级
  )
  azimuth::PriorityBasedSampler::add_sampler(priority_sampler, 
    azimuth::TraceIdRatioBasedSampler::new(0.1), 
    10   // 最低优先级
  )
  
  let trace_id = "trace-12345"
  let span_name = "test-span"
  let span_kind = azimuth::Internal
  let parent_context = azimuth::SpanContext::new(trace_id, "parent-span", true, "")
  
  // 测试匹配高优先级规则的Span
  let critical_error_attrs = [
    ("error.type", azimuth::StringValue("critical")),
    ("service.name", azimuth::StringValue("any-service"))
  ]
  
  let sampling_decision1 = azimuth::Sampler::should_sample(
    priority_sampler,
    parent_context,
    trace_id,
    span_name,
    span_kind,
    critical_error_attrs
  )
  
  assert_true(azimuth::SamplingDecision::is_sampled(sampling_decision1))
  
  // 测试不匹配高优先级规则的Span（应该使用中等优先级采样器）
  let normal_attrs = [
    ("service.name", azimuth::StringValue("normal-service")),
    ("operation.name", azimuth::StringValue("normal-operation"))
  ]
  
  let medium_priority_sampled = 0
  for i in 0..100 {
    let test_trace_id = "trace-" + i.to_string()
    let sampling_decision = azimuth::Sampler::should_sample(
      priority_sampler,
      parent_context,
      test_trace_id,
      span_name,
      span_kind,
      normal_attrs
    )
    
    if (azimuth::SamplingDecision::is_sampled(sampling_decision)) {
      medium_priority_sampled = medium_priority_sampled + 1
    }
  }
  
  // 应该使用中等优先级采样器（80%采样率）
  assert_true(medium_priority_sampled >= 70 && medium_priority_sampled <= 90)
}

pub test "采样性能测试" {
  // 创建不同类型的采样器进行性能比较
  let always_on_sampler = azimuth::AlwaysOnSampler::new()
  let probability_sampler = azimuth::TraceIdRatioBasedSampler::new(0.5)
  let attribute_sampler = azimuth::AttributeBasedSampler::new([("service.name", "test", true)])
  
  let trace_id = "trace-12345"
  let span_name = "test-span"
  let span_kind = azimuth::Internal
  let parent_context = azimuth::SpanContext::new(trace_id, "parent-span", true, "")
  let attrs = [("service.name", azimuth::StringValue("test"))]
  
  // 测试AlwaysOn采样器性能
  let always_on_start = azimuth::Clock::now_unix_nanos(azimuth::Clock::system())
  for i in 0..10000 {
    azimuth::Sampler::should_sample(
      always_on_sampler,
      parent_context,
      trace_id,
      span_name,
      span_kind,
      []
    )
  }
  let always_on_end = azimuth::Clock::now_unix_nanos(azimuth::Clock::system())
  let always_on_duration = always_on_end - always_on_start
  
  // 测试概率采样器性能
  let probability_start = azimuth::Clock::now_unix_nanos(azimuth::Clock::system())
  for i in 0..10000 {
    let test_trace_id = "trace-" + i.to_string()
    azimuth::Sampler::should_sample(
      probability_sampler,
      parent_context,
      test_trace_id,
      span_name,
      span_kind,
      []
    )
  }
  let probability_end = azimuth::Clock::now_unix_nanos(azimuth::Clock::system())
  let probability_duration = probability_end - probability_start
  
  // 测试属性采样器性能
  let attribute_start = azimuth::Clock::now_unix_nanos(azimuth::Clock::system())
  for i in 0..10000 {
    azimuth::Sampler::should_sample(
      attribute_sampler,
      parent_context,
      trace_id,
      span_name,
      span_kind,
      attrs
    )
  }
  let attribute_end = azimuth::Clock::now_unix_nanos(azimuth::Clock::system())
  let attribute_duration = attribute_end - attribute_start
  
  // 验证性能（AlwaysOn应该最快，属性采样器应该最慢）
  assert_true(always_on_duration < probability_duration)
  assert_true(probability_duration < attribute_duration)
  
  // 验证所有采样器都能在合理时间内完成
  assert_true(always_on_duration < 1000000000L)    // 小于1秒
  assert_true(probability_duration < 2000000000L)  // 小于2秒
  assert_true(attribute_duration < 3000000000L)    // 小于3秒
}

pub test "采样决策一致性测试" {
  // 创建确定性采样器
  let deterministic_sampler = azimuth::DeterministicSampler::new(0.5)
  
  let trace_id = "trace-12345"
  let span_name = "test-span"
  let span_kind = azimuth::Internal
  let parent_context = azimuth::SpanContext::new(trace_id, "parent-span", true, "")
  
  // 测试相同输入产生相同决策
  let decisions = []
  for i in 0..10 {
    let sampling_decision = azimuth::Sampler::should_sample(
      deterministic_sampler,
      parent_context,
      trace_id,
      span_name,
      span_kind,
      []
    )
    decisions.push(azimuth::SamplingDecision::is_sampled(sampling_decision))
  }
  
  // 所有决策应该相同
  let first_decision = decisions[0]
  for decision in decisions {
    assert_eq(decision, first_decision)
  }
  
  // 测试不同trace_id产生不同决策
  let different_decisions = []
  for i in 0..100 {
    let test_trace_id = "trace-" + i.to_string()
    let sampling_decision = azimuth::Sampler::should_sample(
      deterministic_sampler,
      parent_context,
      test_trace_id,
      span_name,
      span_kind,
      []
    )
    different_decisions.push(azimuth::SamplingDecision::is_sampled(sampling_decision))
  }
  
  // 应该有大约50%的采样率
  let sampled_count = different_decisions.reduce(0, fn(acc, decision) { if (decision) acc + 1 else acc })
  assert_true(sampled_count >= 40 && sampled_count <= 60)
  
  // 验证采样决策的属性
  for i in 0..10 {
    let test_trace_id = "trace-" + i.to_string()
    let sampling_decision = azimuth::Sampler::should_sample(
      deterministic_sampler,
      parent_context,
      test_trace_id,
      span_name,
      span_kind,
      []
    )
    
    // 验证决策属性
    let attributes = azimuth::SamplingDecision::attributes(sampling_decision)
    assert_true(attributes.length() > 0)
    
    // 验证决策有适当的标签
    let decision = azimuth::SamplingDecision::decision(sampling_decision)
    assert_true(decision == azimuth::RecordAndSample || decision == azimuth::Drop)
  }
}