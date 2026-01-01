// 遥测数据采样策略测试
// 测试各种采样算法、策略和优化方法

pub test "基础概率采样测试" {
  // 创建采样管理器
  let sampling_manager = azimuth::SamplingManager::new()
  
  // 创建概率采样器
  let probability_sampler = azimuth::SamplingManager::create_probability_sampler(sampling_manager, 0.5)  // 50%采样率
  
  // 测试采样决策的一致性
  let trace_id_1 = "trace-1234567890abcdef"
  let trace_id_2 = "trace-fedcba0987654321"
  let trace_id_3 = "trace-1234567890abcdef"  // 与第一个相同
  
  // 相同的trace_id应该得到相同的采样决策
  let decision_1a = azimuth::ProbabilitySampler::should_sample(probability_sampler, trace_id_1)
  let decision_1b = azimuth::ProbabilitySampler::should_sample(probability_sampler, trace_id_1)
  assert_eq(decision_1a, decision_1b)
  
  let decision_2 = azimuth::ProbabilitySampler::should_sample(probability_sampler, trace_id_2)
  let decision_3 = azimuth::ProbabilitySampler::should_sample(probability_sampler, trace_id_3)
  assert_eq(decision_1a, decision_3)  // 应该与第一个相同
  
  // 测试不同采样率的影响
  let low_rate_sampler = azimuth::SamplingManager::create_probability_sampler(sampling_manager, 0.1)   // 10%
  let high_rate_sampler = azimuth::SamplingManager::create_probability_sampler(sampling_manager, 0.9)   // 90%
  
  let sample_count = 1000
  let low_rate_samples = 0
  let high_rate_samples = 0
  
  for i in 0..sample_count {
    let test_trace_id = "trace-" + i.to_string()
    if azimuth::ProbabilitySampler::should_sample(low_rate_sampler, test_trace_id) {
      low_rate_samples = low_rate_samples + 1
    }
    if azimuth::ProbabilitySampler::should_sample(high_rate_sampler, test_trace_id) {
      high_rate_samples = high_rate_samples + 1
    }
  }
  
  // 验证采样率在合理范围内（允许5%的误差）
  let low_rate_actual = low_rate_samples.to_double() / sample_count.to_double()
  let high_rate_actual = high_rate_samples.to_double() / sample_count.to_double()
  
  assert_true(low_rate_actual >= 0.05 && low_rate_actual <= 0.15)
  assert_true(high_rate_actual >= 0.85 && high_rate_actual <= 0.95)
  
  // 测试边界情况
  let zero_sampler = azimuth::SamplingManager::create_probability_sampler(sampling_manager, 0.0)
  let one_sampler = azimuth::SamplingManager::create_probability_sampler(sampling_manager, 1.0)
  
  for i in 0..100 {
    let test_trace_id = "boundary-test-" + i.to_string()
    assert_false(azimuth::ProbabilitySampler::should_sample(zero_sampler, test_trace_id))
    assert_true(azimuth::ProbabilitySampler::should_sample(one_sampler, test_trace_id))
  }
}

pub test "自适应采样策略测试" {
  let sampling_manager = azimuth::SamplingManager::new()
  
  // 创建自适应采样器
  let adaptive_sampler = azimuth::SamplingManager::create_adaptive_sampler(
    sampling_manager, 
    100,  // 目标采样数
    10    // 调整窗口大小
  )
  
  // 模拟低流量情况
  let low_traffic_samples = 0
  for i in 0..50 {
    let trace_id = "low-traffic-" + i.to_string()
    if azimuth::AdaptiveSampler::should_sample(adaptive_sampler, trace_id) {
      low_traffic_samples = low_traffic_samples + 1
    }
  }
  
  // 低流量时应该有更高的采样率
  let low_traffic_rate = low_traffic_samples.to_double() / 50.0
  assert_true(low_traffic_rate > 0.8)  // 大部分应该被采样
  
  // 模拟高流量情况
  let high_traffic_samples = 0
  for i in 0..500 {
    let trace_id = "high-traffic-" + i.to_string()
    if azimuth::AdaptiveSampler::should_sample(adaptive_sampler, trace_id) {
      high_traffic_samples = high_traffic_samples + 1
    }
  }
  
  // 高流量时应该有更低的采样率
  let high_traffic_rate = high_traffic_samples.to_double() / 500.0
  assert_true(high_traffic_rate < 0.3)  // 少部分应该被采样
  
  // 测试自适应调整
  let current_rate = azimuth::AdaptiveSampler::get_current_sampling_rate(adaptive_sampler)
  assert_true(current_rate > 0.0 && current_rate <= 1.0)
  
  // 测试采样统计
  let sampling_stats = azimuth::AdaptiveSampler::get_sampling_statistics(adaptive_sampler)
  assert_true(sampling_stats.total_traces >= 550)
  assert_true(sampling_stats.sampled_traces >= 0)
  assert_eq(sampling_stats.total_traces, sampling_stats.sampled_traces + sampling_stats.dropped_traces)
}

pub test "基于属性的采样测试" {
  let sampling_manager = azimuth::SamplingManager::new()
  
  // 创建基于属性的采样器
  let attribute_sampler = azimuth::SamplingManager::create_attribute_based_sampler(sampling_manager)
  
  // 设置采样规则
  azimuth::AttributeSampler::add_rule(attribute_sampler, {
    name: "high-priority-rules",
    conditions: [
      { attribute: "http.status_code", operator: ">=", value: 500 },
      { attribute: "error.type", operator: "exists", value: null },
      { attribute: "service.name", operator: "equals", value: "critical-service" }
    ],
    sampling_rate: 1.0  // 总是采样
  })
  
  azimuth::AttributeSampler::add_rule(attribute_sampler, {
    name: "medium-priority-rules",
    conditions: [
      { attribute: "http.method", operator: "in", value: ["POST", "PUT", "DELETE"] },
      { attribute: "user.premium", operator: "equals", value: "true" }
    ],
    sampling_rate: 0.5  // 50%采样
  })
  
  azimuth::AttributeSampler::add_rule(attribute_sampler, {
    name: "low-priority-rules",
    conditions: [
      { attribute: "http.method", operator: "equals", value: "GET" }
    ],
    sampling_rate: 0.1  // 10%采样
  })
  
  // 测试高优先级规则
  let error_attrs = azimuth::Attributes::new()
  azimuth::Attributes::set(error_attrs, "http.status_code", azimuth::IntValue(500))
  azimuth::Attributes::set(error_attrs, "error.type", azimuth::StringValue("timeout"))
  
  let error_decision = azimuth::AttributeSampler::should_sample_with_attributes(
    attribute_sampler, 
    "error-trace", 
    error_attrs
  )
  assert_true(error_decision)  // 应该总是采样
  
  // 测试中等优先级规则
  let premium_attrs = azimuth::Attributes::new()
  azimuth::Attributes::set(premium_attrs, "http.method", azimuth::StringValue("POST"))
  azimuth::Attributes::set(premium_attrs, "user.premium", azimuth::StringValue("true"))
  
  let premium_sample_count = 0
  for i in 0..100 {
    let trace_id = "premium-trace-" + i.to_string()
    if azimuth::AttributeSampler::should_sample_with_attributes(attribute_sampler, trace_id, premium_attrs) {
      premium_sample_count = premium_sample_count + 1
    }
  }
  
  let premium_rate = premium_sample_count.to_double() / 100.0
  assert_true(premium_rate >= 0.4 && premium_rate <= 0.6)  // 约50%
  
  // 测试低优先级规则
  let get_attrs = azimuth::Attributes::new()
  azimuth::Attributes::set(get_attrs, "http.method", azimuth::StringValue("GET"))
  
  let get_sample_count = 0
  for i in 0..100 {
    let trace_id = "get-trace-" + i.to_string()
    if azimuth::AttributeSampler::should_sample_with_attributes(attribute_sampler, trace_id, get_attrs) {
      get_sample_count = get_sample_count + 1
    }
  }
  
  let get_rate = get_sample_count.to_double() / 100.0
  assert_true(get_rate >= 0.05 && get_rate <= 0.15)  // 约10%
  
  // 测试无匹配规则的情况
  let no_rule_attrs = azimuth::Attributes::new()
  azimuth::Attributes::set(no_rule_attrs, "custom.attribute", azimuth::StringValue("value"))
  
  let no_rule_decision = azimuth::AttributeSampler::should_sample_with_attributes(
    attribute_sampler, 
    "no-rule-trace", 
    no_rule_attrs
  )
  assert_false(no_rule_decision)  // 默认不采样
}

pub test "限流采样策略测试" {
  let sampling_manager = azimuth::SamplingManager::new()
  
  // 创建限流采样器
  let rate_limiter = azimuth::SamplingManager::create_rate_limiting_sampler(sampling_manager, 10)  // 每秒10个
  
  // 测试限流功能
  let sampled_count = 0
  let total_requests = 50
  
  // 在短时间内发送大量请求
  for i in 0..total_requests {
    let trace_id = "rate-limit-" + i.to_string()
    if azimuth::RateLimitingSampler::should_sample(rate_limiter, trace_id) {
      sampled_count = sampled_count + 1
    }
  }
  
  // 应该被限制在设定范围内
  assert_true(sampled_count <= 15)  // 允许一些误差
  
  // 测试时间窗口重置
  let reset_time = azimuth::Clock::now_unix_nanos(azimuth::Clock::system()) + 1000000000L  // 1秒后
  azimuth::RateLimitingSampler::set_time(rate_limiter, reset_time)
  
  let after_reset_samples = 0
  for i in 0..20 {
    let trace_id = "after-reset-" + i.to_string()
    if azimuth::RateLimitingSampler::should_sample(rate_limiter, trace_id) {
      after_reset_samples = after_reset_samples + 1
    }
  }
  
  // 时间窗口重置后应该重新开始计数
  assert_true(after_reset_samples <= 12)
  
  // 测试突发处理
  let burst_sampler = azimuth::SamplingManager::create_burst_rate_limiter(sampling_manager, 5, 20)  // 平均5，突发20
  let burst_samples = 0
  
  for i in 0..30 {
    let trace_id = "burst-" + i.to_string()
    if azimuth::BurstRateLimiter::should_sample(burst_sampler, trace_id) {
      burst_samples = burst_samples + 1
    }
  }
  
  // 应该允许突发请求
  assert_true(burst_samples >= 15 && burst_samples <= 25)
  
  // 测试限流统计
  let rate_stats = azimuth::RateLimitingSampler::get_statistics(rate_limiter)
  assert_true(rate_stats.total_requests >= total_requests)
  assert_true(rate_stats.sampled_requests <= rate_stats.total_requests)
  assert_true(rate_stats.dropped_requests >= 0)
}

pub test "分层采样策略测试" {
  let sampling_manager = azimuth::SamplingManager::new()
  
  // 创建分层采样器
  let tiered_sampler = azimuth::SamplingManager::create_tiered_sampler(sampling_manager)
  
  // 设置不同层级
  azimuth::TieredSampler::add_tier(tiered_sampler, {
    name: "root-tier",
    sampling_rate: 1.0,  // 根层级总是采样
    priority: 1
  })
  
  azimuth::TieredSampler::add_tier(tiered_sampler, {
    name: "service-tier",
    sampling_rate: 0.5,  # 服务层级50%采样
    priority: 2
  })
  
  azimuth::TieredSampler::add_tier(tiered_sampler, {
    name: "operation-tier",
    sampling_rate: 0.2,  # 操作层级20%采样
    priority: 3
  })
  
  // 测试根层级采样
  let root_decision = azimuth::TieredSampler::should_sample_at_tier(
    tiered_sampler, 
    "root-trace", 
    "root-tier"
  )
  assert_true(root_decision)
  
  // 测试服务层级采样
  let service_sample_count = 0
  for i in 0..100 {
    let trace_id = "service-trace-" + i.to_string()
    if azimuth::TieredSampler::should_sample_at_tier(tiered_sampler, trace_id, "service-tier") {
      service_sample_count = service_sample_count + 1
    }
  }
  
  let service_rate = service_sample_count.to_double() / 100.0
  assert_true(service_rate >= 0.4 && service_rate <= 0.6)
  
  // 测试操作层级采样
  let operation_sample_count = 0
  for i in 0..100 {
    let trace_id = "operation-trace-" + i.to_string()
    if azimuth::TieredSampler::should_sample_at_tier(tiered_sampler, trace_id, "operation-tier") {
      operation_sample_count = operation_sample_count + 1
    }
  }
  
  let operation_rate = operation_sample_count.to_double() / 100.0
  assert_true(operation_rate >= 0.15 && operation_rate <= 0.25)
  
  // 测试层级依赖关系
  let parent_trace_id = "parent-trace-123"
  let child_trace_id = "child-trace-456"
  
  // 父级被采样时，子级应该有更高的采样概率
  let parent_sampled = azimuth::TieredSampler::should_sample_at_tier(tiered_sampler, parent_trace_id, "root-tier")
  let child_with_parent_sampled = azimuth::TieredSampler::should_sample_with_parent(
    tiered_sampler, 
    child_trace_id, 
    "service-tier", 
    parent_trace_id
  )
  
  if parent_sampled {
    assert_true(child_with_parent_sampled)  # 父级被采样时，子级应该被采样
  }
  
  // 测试层级统计
  let tier_stats = azimuth::TieredSampler::get_tier_statistics(tiered_sampler)
  assert_true(tier_stats.length() >= 3)
  
  let root_stats = tier_stats.find(fn(stat) { stat.tier_name == "root-tier" })
  let service_stats = tier_stats.find(fn(stat) { stat.tier_name == "service-tier" })
  let operation_stats = tier_stats.find(fn(stat) { stat.tier_name == "operation-tier" })
  
  assert_true(root_stats.total_traces >= 1)
  assert_true(service_stats.total_traces >= 100)
  assert_true(operation_stats.total_traces >= 100)
}

pub test "动态采样调整测试" {
  let sampling_manager = azimuth::SamplingManager::new()
  
  // 创建动态采样器
  let dynamic_sampler = azimuth::SamplingManager::create_dynamic_sampler(sampling_manager, 0.1)  // 初始10%
  
  // 模拟系统负载变化
  let load_metrics = [
    { timestamp: 1000L, cpu_usage: 0.3, memory_usage: 0.4, request_rate: 100 },
    { timestamp: 2000L, cpu_usage: 0.7, memory_usage: 0.8, request_rate: 500 },
    { timestamp: 3000L, cpu_usage: 0.9, memory_usage: 0.9, request_rate: 1000 },
    { timestamp: 4000L, cpu_usage: 0.5, memory_usage: 0.6, request_rate: 300 },
    { timestamp: 5000L, cpu_usage: 0.2, memory_usage: 0.3, request_rate: 50 }
  ]
  
  // 测试采样率动态调整
  for metric in load_metrics {
    azimuth::DynamicSampler::update_load_metrics(dynamic_sampler, metric)
    let adjusted_rate = azimuth::DynamicSampler::get_current_rate(dynamic_sampler)
    
    // 高负载时应该降低采样率
    if metric.cpu_usage > 0.8 || metric.memory_usage > 0.8 || metric.request_rate > 800 {
      assert_true(adjusted_rate < 0.1)
    }
    // 低负载时可以提高采样率
    else if metric.cpu_usage < 0.3 && metric.memory_usage < 0.4 && metric.request_rate < 100 {
      assert_true(adjusted_rate > 0.05)
    }
  }
  
  // 测试采样率历史记录
  let rate_history = azimuth::DynamicSampler::get_rate_history(dynamic_sampler)
  assert_true(rate_history.length() >= 5)
  
  // 验证采样率变化趋势
  for i in 1..rate_history.length() {
    let prev_rate = rate_history[i-1].rate
    let curr_rate = rate_history[i].rate
    let prev_load = rate_history[i-1].load_score
    let curr_load = rate_history[i].load_score
    
    // 负载增加时，采样率应该降低
    if curr_load > prev_load {
      assert_true(curr_rate <= prev_rate)
    }
    // 负载减少时，采样率可以提高
    else if curr_load < prev_load {
      assert_true(curr_rate >= prev_rate)
    }
  }
  
  // 测试手动采样率调整
  azimuth::DynamicSampler::set_manual_rate(dynamic_sampler, 0.25)
  let manual_rate = azimuth::DynamicSampler::get_current_rate(dynamic_sampler)
  assert_eq(manual_rate, 0.25)
  
  // 测试自动模式切换
  azimuth::DynamicSampler::enable_auto_mode(dynamic_sampler, true)
  let auto_enabled = azimuth::DynamicSampler::is_auto_mode_enabled(dynamic_sampler)
  assert_true(auto_enabled)
  
  // 添加新的负载指标，应该自动调整
  azimuth::DynamicSampler::update_load_metrics(dynamic_sampler, {
    timestamp: 6000L,
    cpu_usage: 0.95,
    memory_usage: 0.95,
    request_rate: 1500
  })
  
  let auto_adjusted_rate = azimuth::DynamicSampler::get_current_rate(dynamic_sampler)
  assert_true(auto_adjusted_rate < 0.25)  # 应该自动降低
}

pub test "采样一致性保证测试" {
  let sampling_manager = azimuth::SamplingManager::new()
  
  // 创建一致性采样器
  let consistent_sampler = azimuth::SamplingManager::create_consistent_sampler(sampling_manager, 0.1)
  
  // 测试相同trace_id的一致性采样
  let trace_id = "consistency-test-trace-12345"
  
  let decisions = []
  for i in 0..100 {
    let decision = azimuth::ConsistentSampler::should_sample(consistent_sampler, trace_id)
    decisions.push(decision)
  }
  
  // 所有决策应该一致
  let first_decision = decisions[0]
  for decision in decisions {
    assert_eq(decision, first_decision)
  }
  
  // 测试相关trace_id的采样关联性
  let parent_trace_id = "parent-trace-123"
  let child_trace_ids = [
    "parent-trace-123-child-1",
    "parent-trace-123-child-2", 
    "parent-trace-123-child-3"
  ]
  
  let parent_decision = azimuth::ConsistentSampler::should_sample(consistent_sampler, parent_trace_id)
  let child_decisions = []
  
  for child_id in child_trace_ids {
    let child_decision = azimuth::ConsistentSampler::should_sample_with_parent(
      consistent_sampler, 
      child_id, 
      parent_trace_id
    )
    child_decisions.push(child_decision)
  }
  
  // 子trace的采样决策应该与父trace一致
  for child_decision in child_decisions {
    assert_eq(child_decision, parent_decision)
  }
  
  // 测试采样决策的可重现性
  let sampler_1 = azimuth::SamplingManager::create_consistent_sampler(sampling_manager, 0.1)
  let sampler_2 = azimuth::SamplingManager::create_consistent_sampler(sampling_manager, 0.1)
  
  let test_trace_ids = ["trace-a", "trace-b", "trace-c", "trace-d", "trace-e"]
  
  for trace_id in test_trace_ids {
    let decision_1 = azimuth::ConsistentSampler::should_sample(sampler_1, trace_id)
    let decision_2 = azimuth::ConsistentSampler::should_sample(sampler_2, trace_id)
    assert_eq(decision_1, decision_2)  # 相同的采样器应该产生相同的决策
  }
  
  // 测试采样决策的分布均匀性
  let uniform_test_count = 10000
  let sampled_count = 0
  
  for i in 0..uniform_test_count {
    let test_trace_id = "uniform-test-" + i.to_string()
    if azimuth::ConsistentSampler::should_sample(consistent_sampler, test_trace_id) {
      sampled_count = sampled_count + 1
    }
  }
  
  let actual_rate = sampled_count.to_double() / uniform_test_count.to_double()
  assert_true(actual_rate >= 0.09 && actual_rate <= 0.11)  # 应该接近10%
  
  // 测试采样决策的稳定性
  let stability_test_traces = ["stability-1", "stability-2", "stability-3"]
  let initial_decisions = {}
  
  for trace_id in stability_test_traces {
    initial_decisions[trace_id] = azimuth::ConsistentSampler::should_sample(consistent_sampler, trace_id)
  }
  
  // 模拟时间流逝和系统状态变化
  for i in 0..1000 {
    let temp_trace_id = "temp-" + i.to_string()
    azimuth::ConsistentSampler::should_sample(consistent_sampler, temp_trace_id)
  }
  
  // 验证原始trace的决策保持不变
  for trace_id in stability_test_traces {
    let current_decision = azimuth::ConsistentSampler::should_sample(consistent_sampler, trace_id)
    assert_eq(current_decision, initial_decisions[trace_id])
  }
}

pub test "采样性能和效率测试" {
  let sampling_manager = azimuth::SamplingManager::new()
  
  // 创建不同类型的采样器进行性能比较
  let probability_sampler = azimuth::SamplingManager::create_probability_sampler(sampling_manager, 0.1)
  let attribute_sampler = azimuth::SamplingManager::create_attribute_based_sampler(sampling_manager)
  let adaptive_sampler = azimuth::SamplingManager::create_adaptive_sampler(sampling_manager, 100, 10)
  
  // 为属性采样器添加简单规则
  azimuth::AttributeSampler::add_rule(attribute_sampler, {
    name: "performance-test-rule",
    conditions: [{ attribute: "test.attr", operator: "exists", value: null }],
    sampling_rate: 0.1
  })
  
  let test_attrs = azimuth::Attributes::new()
  azimuth::Attributes::set(test_attrs, "test.attr", azimuth::StringValue("value"))
  
  // 性能测试参数
  let performance_test_count = 10000
  let test_trace_ids = []
  
  for i in 0..performance_test_count {
    test_trace_ids.push("perf-test-" + i.to_string())
  }
  
  // 测试概率采样器性能
  let prob_start_time = azimuth::Clock::now_unix_nanos(azimuth::Clock::system())
  
  for trace_id in test_trace_ids {
    azimuth::ProbabilitySampler::should_sample(probability_sampler, trace_id)
  }
  
  let prob_end_time = azimuth::Clock::now_unix_nanos(azimuth::Clock::system())
  let prob_duration = prob_end_time - prob_start_time
  
  // 测试属性采样器性能
  let attr_start_time = azimuth::Clock::now_unix_nanos(azimuth::Clock::system())
  
  for trace_id in test_trace_ids {
    azimuth::AttributeSampler::should_sample_with_attributes(attribute_sampler, trace_id, test_attrs)
  }
  
  let attr_end_time = azimuth::Clock::now_unix_nanos(azimuth::Clock::system())
  let attr_duration = attr_end_time - attr_start_time
  
  // 测试自适应采样器性能
  let adaptive_start_time = azimuth::Clock::now_unix_nanos(azimuth::Clock::system())
  
  for trace_id in test_trace_ids {
    azimuth::AdaptiveSampler::should_sample(adaptive_sampler, trace_id)
  }
  
  let adaptive_end_time = azimuth::Clock::now_unix_nanos(azimuth::Clock::system())
  let adaptive_duration = adaptive_end_time - adaptive_start_time
  
  // 验证性能要求
  let max_allowed_duration = 1000000000L  // 1秒
  
  assert_true(prob_duration < max_allowed_duration)
  assert_true(attr_duration < max_allowed_duration)
  assert_true(adaptive_duration < max_allowed_duration)
  
  // 概率采样器应该是最快的
  assert_true(prob_duration <= attr_duration)
  assert_true(prob_duration <= adaptive_duration)
  
  // 测试内存使用效率
  let prob_memory = azimuth::SamplingManager::get_memory_usage(probability_sampler)
  let attr_memory = azimuth::SamplingManager::get_memory_usage(attribute_sampler)
  let adaptive_memory = azimuth::SamplingManager::get_memory_usage(adaptive_sampler)
  
  // 验证内存使用在合理范围内
  assert_true(prob_memory < 1024 * 1024)   # 小于1MB
  assert_true(attr_memory < 10 * 1024 * 1024)  # 小于10MB
  assert_true(adaptive_memory < 5 * 1024 * 1024)  # 小于5MB
  
  // 测试并发采样性能
  let concurrent_sampler = azimuth::SamplingManager::create_concurrent_safe_sampler(sampling_manager, 0.1)
  let concurrent_threads = 10
  let operations_per_thread = 1000
  
  let concurrent_start_time = azimuth::Clock::now_unix_nanos(azimuth::Clock::system())
  
  // 模拟并发采样
  for thread_id in 0..concurrent_threads {
    for op_id in 0..operations_per_thread {
      let trace_id = "concurrent-" + thread_id.to_string + "-" + op_id.to_string
      azimuth::ConcurrentSampler::should_sample(concurrent_sampler, trace_id)
    }
  }
  
  let concurrent_end_time = azimuth::Clock::now_unix_nanos(azimuth::Clock::system())
  let concurrent_duration = concurrent_end_time - concurrent_start_time
  
  // 并发性能应该合理
  let total_concurrent_ops = concurrent_threads * operations_per_thread
  let avg_concurrent_time = concurrent_duration / total_concurrent_ops.to_long()
  
  assert_true(avg_concurrent_time < 100000)  # 平均每次操作小于100微秒
  
  // 验证并发采样的一致性
  let concurrent_stats = azimuth::ConcurrentSampler::get_statistics(concurrent_sampler)
  assert_true(concurrent_stats.total_operations >= total_concurrent_ops)
  assert_true(concurrent_stats.concurrent_errors == 0)
}