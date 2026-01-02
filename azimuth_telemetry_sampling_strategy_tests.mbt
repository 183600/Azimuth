// Azimuth Telemetry Sampling Strategy Tests
// 遥测数据采样策略测试用例，专注于各种采样算法和策略

// Test 1: 固定比例采样策略
test "fixed ratio sampling strategy" {
  // 创建固定比例采样器，采样率为10%
  let sampler = azimuth::sampler::FixedRatio::new(0.1)
  
  // 模拟100个追踪决策
  let mut sampled_count = 0
  let trace_id_prefix = "trace1234567890123456789012"
  
  for i in 0..=99 {
    let trace_id = trace_id_prefix + i.to_string()
    let decision = azimuth::sampler::should_sample(sampler, trace_id, "test_span")
    
    if decision {
      sampled_count = sampled_count + 1
    }
  }
  
  // 验证采样数量在合理范围内（8-12个）
  assert_true(sampled_count >= 8 && sampled_count <= 12)
}

// Test 2: 基于追踪ID的概率采样
test "trace ID based probabilistic sampling" {
  // 创建概率采样器
  let sampler = azimuth::sampler::Probabilistic::new(0.05) // 5%采样率
  
  // 使用不同的追踪ID测试
  let trace_ids = [
    "abc123def45678901234567890123456",
    "def456abc12378901234567890123456",
    "789012abc123def45678901234567890",
    "1234567890abcdef1234567890abcdef",
    "abcdef12345678901234567890123456"
  ]
  
  let mut results = []
  
  for trace_id in trace_ids {
    let decision = azimuth::sampler::should_sample(sampler, trace_id, "test_span")
    results = results.push(decision)
  }
  
  // 验证相同追踪ID的决策一致性
  let first_decision = results[0]
  let same_decision = azimuth::sampler::should_sample(sampler, trace_ids[0], "test_span")
  assert_eq(first_decision, same_decision)
}

// Test 3: 基于属性的自适应采样
test "attribute based adaptive sampling" {
  // 创建自适应采样器
  let sampler = azimuth::sampler::Adaptive::new()
  
  // 配置采样规则
  azimuth::sampler::add_rule(sampler, "error", true, 1.0) // 错误追踪100%采样
  azimuth::sampler::add_rule(sampler, "high_priority", true, 0.8) // 高优先级80%采样
  azimuth::sampler::add_rule(sampler, "normal", true, 0.1) // 普通追踪10%采样
  
  // 创建测试属性
  let error_attrs = azimuth::Attributes::new()
  azimuth::Attributes::set(error_attrs, "error", true)
  
  let high_priority_attrs = azimuth::Attributes::new()
  azimuth::Attributes::set(high_priority_attrs, "high_priority", true)
  
  let normal_attrs = azimuth::Attributes::new()
  azimuth::Attributes::set(normal_attrs, "normal", true)
  
  // 测试不同属性的采样决策
  let error_decision = azimuth::sampler::should_sample_with_attributes(
    sampler, "trace1", "error_span", error_attrs
  )
  let high_priority_decision = azimuth::sampler::should_sample_with_attributes(
    sampler, "trace2", "priority_span", high_priority_attrs
  )
  let normal_decision = azimuth::sampler::should_sample_with_attributes(
    sampler, "trace3", "normal_span", normal_attrs
  )
  
  // 验证采样决策符合预期
  assert_true(error_decision) // 错误追踪应该总是被采样
  // 高优先级和普通追踪的采样率需要多次测试验证
}

// Test 4: 动态采样率调整
test "dynamic sampling rate adjustment" {
  // 创建动态采样器
  let sampler = azimuth::sampler::Dynamic::new(0.1) // 初始10%采样率
  
  // 模拟系统负载变化
  let initial_rate = azimuth::sampler::get_current_rate(sampler)
  assert_eq(initial_rate, 0.1)
  
  // 系统负载增加，降低采样率
  azimuth::sampler::adjust_rate(sampler, 0.05)
  let adjusted_rate = azimuth::sampler::get_current_rate(sampler)
  assert_eq(adjusted_rate, 0.05)
  
  // 系统负载降低，提高采样率
  azimuth::sampler::adjust_rate(sampler, 0.2)
  let increased_rate = azimuth::sampler::get_current_rate(sampler)
  assert_eq(increased_rate, 0.2)
}

// Test 5: 采样决策的父子追踪一致性
test "parent-child span sampling consistency" {
  // 创建采样器
  let sampler = azimuth::sampler::Probabilistic::new(0.2)
  
  // 父追踪决策
  let parent_trace_id = "parent1234567890123456789012"
  let parent_decision = azimuth::sampler::should_sample(sampler, parent_trace_id, "parent_span")
  
  // 子追踪应该遵循父追踪的采样决策
  let child_decision = azimuth::sampler::should_sample_child(
    sampler, parent_trace_id, "child_span", parent_decision
  )
  
  assert_eq(parent_decision, child_decision)
}

// Test 6: 采样统计和监控
test "sampling statistics and monitoring" {
  // 创建带统计功能的采样器
  let sampler = azimuth::sampler::Monitored::new(0.15)
  
  // 模拟采样决策
  for i in 0..=99 {
    let trace_id = "stat_test" + i.to_string()
    azimuth::sampler::should_sample(sampler, trace_id, "test_span")
  }
  
  // 获取采样统计
  let stats = azimuth::sampler::get_statistics(sampler)
  let total_decisions = azimuth::sampler::stats::total_decisions(stats)
  let sampled_decisions = azimuth::sampler::stats::sampled_decisions(stats)
  let actual_rate = azimuth::sampler::stats::actual_sampling_rate(stats)
  
  assert_eq(total_decisions, 100)
  assert_true(sampled_decisions >= 10 && sampled_decisions <= 20)
  assert_true(actual_rate >= 0.1 && actual_rate <= 0.25)
}

// Test 7: 采样器组合策略
test "sampler composition strategies" {
  // 创建多个采样器
  let probabilistic_sampler = azimuth::sampler::Probabilistic::new(0.1)
  let adaptive_sampler = azimuth::sampler::Adaptive::new()
  let rate_limit_sampler = azimuth::sampler::RateLimit::new(10) // 每秒最多10个采样
  
  // 配置自适应采样规则
  azimuth::sampler::add_rule(adaptive_sampler, "critical", true, 1.0)
  
  // 创建组合采样器
  let composed_sampler = azimuth::sampler::Composite::new([
    ("adaptive", adaptive_sampler),
    ("probabilistic", probabilistic_sampler),
    ("rate_limit", rate_limit_sampler)
  ])
  
  // 测试关键追踪（应该被自适应采样器捕获）
  let critical_attrs = azimuth::Attributes::new()
  azimuth::Attributes::set(critical_attrs, "critical", true)
  
  let critical_decision = azimuth::sampler::should_sample_with_attributes(
    composed_sampler, "critical_trace", "critical_span", critical_attrs
  )
  assert_true(critical_decision)
  
  // 测试普通追踪（使用概率采样）
  let normal_decision = azimuth::sampler::should_sample(
    composed_sampler, "normal_trace", "normal_span"
  )
  // 普通追踪的采样率应该接近概率采样器的设置
}

// Test 8: 采样边界条件测试
test "sampling edge cases" {
  // 测试0%采样率
  let zero_sampler = azimuth::sampler::FixedRatio::new(0.0)
  for i in 0..=9 {
    let decision = azimuth::sampler::should_sample(zero_sampler, "trace" + i.to_string(), "span")
    assert_false(decision)
  }
  
  // 测试100%采样率
  let full_sampler = azimuth::sampler::FixedRatio::new(1.0)
  for i in 0..=9 {
    let decision = azimuth::sampler::should_sample(full_sampler, "trace" + i.to_string(), "span")
    assert_true(decision)
  }
  
  // 测试无效采样率
  let invalid_sampler = azimuth::sampler::FixedRatio::new(-0.1) // 应该被调整为0.0
  let invalid_rate = azimuth::sampler::get_current_rate(invalid_sampler)
  assert_eq(invalid_rate, 0.0)
  
  let overflow_sampler = azimuth::sampler::FixedRatio::new(1.5) // 应该被调整为1.0
  let overflow_rate = azimuth::sampler::get_current_rate(overflow_sampler)
  assert_eq(overflow_rate, 1.0)
}