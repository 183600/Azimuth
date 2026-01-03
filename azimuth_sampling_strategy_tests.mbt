// Azimuth Sampling Strategy and Algorithm Tests
// 采样算法和策略测试用例 - 专注于采样率、采样策略和采样性能

// Test 1: 基础概率采样测试
test "basic probability sampling" {
  // 创建概率采样器（10%采样率）
  let sampler = ProbabilitySampler::new(0.1)
  
  // 测试采样决策
  let mut sampled_count = 0
  let total_samples = 10000
  
  for i in 0..<total_samples {
    let trace_id = "trace_" + i.to_string()
    let decision = Sampler::should_sample(sampler, trace_id)
    
    if decision {
      sampled_count = sampled_count + 1
    }
  }
  
  // 验证采样率接近预期（允许5%的误差）
  let actual_rate = sampled_count.to_float() / total_samples.to_float()
  assert_true(actual_rate > 0.05 && actual_rate < 0.15)
  
  // 测试边界情况
  let always_sampler = ProbabilitySampler::new(1.0) // 100%采样
  assert_true(Sampler::should_sample(always_sampler, "test_trace"))
  
  let never_sampler = ProbabilitySampler::new(0.0) // 0%采样
  assert_false(Sampler::should_sample(never_sampler, "test_trace"))
  
  // 测试相同trace_id的采样决策一致性
  let trace_id = "consistency_test_trace"
  let decision1 = Sampler::should_sample(sampler, trace_id)
  let decision2 = Sampler::should_sample(sampler, trace_id)
  
  // 相同的trace_id应该得到相同的采样决策
  assert_eq(decision1, decision2)
}

// Test 2: 自适应采样策略测试
test "adaptive sampling strategy" {
  // 创建自适应采样器
  let adaptive_sampler = AdaptiveSampler::new(
    0.1, // 初始采样率10%
    0.01, // 最小采样率1%
    1.0, // 最大采样率100%
    100, // 目标采样数量
    1000 // 调整窗口大小
  )
  
  // 模拟低流量情况
  for i in 0..<50 {
    let trace_id = "low_traffic_trace_" + i.to_string()
    Sampler::should_sample(adaptive_sampler, trace_id)
  }
  
  // 在低流量情况下，采样率应该提高
  let low_traffic_rate = AdaptiveSampler::get_current_rate(adaptive_sampler)
  assert_true(low_traffic_rate > 0.1)
  
  // 模拟高流量情况
  for i in 0..<1000 {
    let trace_id = "high_traffic_trace_" + i.to_string()
    Sampler::should_sample(adaptive_sampler, trace_id)
  }
  
  // 在高流量情况下，采样率应该降低
  let high_traffic_rate = AdaptiveSampler::get_current_rate(adaptive_sampler)
  assert_true(high_traffic_rate < low_traffic_rate)
  assert_true(high_traffic_rate >= 0.01) // 不应低于最小采样率
  
  // 测试采样率调整
  AdaptiveSampler::set_target_sample_count(adaptive_sampler, 50)
  AdaptiveSampler::adjust_rate(adaptive_sampler)
  
  let adjusted_rate = AdaptiveSampler::get_current_rate(adaptive_sampler)
  assert_true(adjusted_rate != high_traffic_rate)
}

// Test 3: 基于属性的采样测试
test "attribute-based sampling" {
  // 创建基于属性的采样器
  let attr_sampler = AttributeSampler::new()
  
  // 添加采样规则
  AttributeSampler::add_rule(attr_sampler, "error", true, 1.0) // 错误请求100%采样
  AttributeSampler::add_rule(attr_sampler, "http.status_code", 500, 1.0) // 5xx错误100%采样
  AttributeSampler::add_rule(attr_sampler, "service.name", "critical-service", 0.5) // 关键服务50%采样
  AttributeSampler::add_rule(attr_sampler, "user.id", "", 0.01) // 默认1%采样
  
  // 测试错误请求采样
  let error_attrs = Attributes::new()
  Attributes::set(error_attrs, "error", BoolValue(true))
  
  let error_decision = AttributeSampler::should_sample(attr_sampler, "error_trace", error_attrs)
  assert_true(error_decision)
  
  // 测试5xx状态码采样
  let status_attrs = Attributes::new()
  Attributes::set(status_attrs, "http.status_code", IntValue(500))
  
  let status_decision = AttributeSampler::should_sample(attr_sampler, "status_trace", status_attrs)
  assert_true(status_decision)
  
  // 测试关键服务采样
  let service_attrs = Attributes::new()
  Attributes::set(service_attrs, "service.name", StringValue("critical-service"))
  
  let service_decision = AttributeSampler::should_sample(attr_sampler, "service_trace", service_attrs)
  // 50%采样率，可能采样也可能不采样
  
  // 测试默认采样率
  let default_attrs = Attributes::new()
  Attributes::set(default_attrs, "user.id", StringValue("user123"))
  
  let default_decision = AttributeSampler::should_sample(attr_sampler, "default_trace", default_attrs)
  // 1%采样率，可能采样也可能不采样
  
  // 测试多属性匹配（优先级）
  let multi_attrs = Attributes::new()
  Attributes::set(multi_attrs, "error", BoolValue(true))
  Attributes::set(multi_attrs, "service.name", StringValue("critical-service"))
  
  let multi_decision = AttributeSampler::should_sample(attr_sampler, "multi_trace", multi_attrs)
  assert_true(multi_decision) // 错误规则应该优先
}

// Test 4: 限流采样测试
test "rate limiting sampling" {
  // 创建限流采样器（每秒最多100个样本）
  let rate_limiter = RateLimitingSampler::new(100, 1.0) // 100样本/秒
  
  // 测试在限制内的采样
  let mut sampled_count = 0
  for i in 0..<50 {
    let trace_id = "within_limit_trace_" + i.to_string()
    if Sampler::should_sample(rate_limiter, trace_id) {
      sampled_count = sampled_count + 1
    }
  }
  
  // 在限制内，所有请求都应该被采样
  assert_eq(sampled_count, 50)
  
  // 测试超过限制的采样
  sampled_count = 0
  for i in 0..<200 {
    let trace_id = "over_limit_trace_" + i.to_string()
    if Sampler::should_sample(rate_limiter, trace_id) {
      sampled_count = sampled_count + 1
    }
  }
  
  // 超过限制，只有部分请求应该被采样
  assert_true(sampled_count <= 100)
  assert_true(sampled_count > 0)
  
  // 等待1秒后，限制应该重置
  Time::sleep(1000)
  
  sampled_count = 0
  for i in 0..<50 {
    let trace_id = "after_reset_trace_" + i.to_string()
    if Sampler::should_sample(rate_limiter, trace_id) {
      sampled_count = sampled_count + 1
    }
  }
  
  // 重置后，应该又能采样
  assert_eq(sampled_count, 50)
  
  // 测试突发处理
  let burst_sampler = RateLimitingSampler::with_burst(100, 1.0, 200) // 100样本/秒，突发200
  
  sampled_count = 0
  for i in 0..<150 {
    let trace_id = "burst_trace_" + i.to_string()
    if Sampler::should_sample(burst_sampler, trace_id) {
      sampled_count = sampled_count + 1
    }
  }
  
  // 突发情况下，应该能处理更多请求
  assert_true(sampled_count > 100)
  assert_true(sampled_count <= 200)
}

// Test 5: 组合采样策略测试
test "composite sampling strategy" {
  // 创建子采样器
  let probability_sampler = ProbabilitySampler::new(0.1) // 10%概率采样
  let attribute_sampler = AttributeSampler::new()
  AttributeSampler::add_rule(attribute_sampler, "error", true, 1.0) // 错误100%采样
  
  // 创建组合采样器（AND逻辑）
  let and_sampler = CompositeSampler::and([probability_sampler, attribute_sampler])
  
  // 测试错误请求（应该被属性采样器选中，但可能被概率采样器拒绝）
  let error_attrs = Attributes::new()
  Attributes::set(error_attrs, "error", BoolValue(true))
  
  let mut error_sampled_count = 0
  for i in 0..<100 {
    let trace_id = "error_trace_" + i.to_string()
    if Sampler::should_sample(and_sampler, trace_id, Some(error_attrs)) {
      error_sampled_count = error_sampled_count + 1
    }
  }
  
  // 错误请求应该被属性采样器选中，但只有约10%能通过概率采样
  let error_rate = error_sampled_count.to_float() / 100.0
  assert_true(error_rate > 0.05 && error_rate < 0.15)
  
  // 创建组合采样器（OR逻辑）
  let or_sampler = CompositeSampler::or([probability_sampler, attribute_sampler])
  
  // 测试错误请求（应该被属性采样器选中，OR逻辑应该直接通过）
  let mut or_error_sampled_count = 0
  for i in 0..<100 {
    let trace_id = "or_error_trace_" + i.to_string()
    if Sampler::should_sample(or_sampler, trace_id, Some(error_attrs)) {
      or_error_sampled_count = or_error_sampled_count + 1
    }
  }
  
  // 错误请求应该全部被采样
  assert_eq(or_error_sampled_count, 100)
  
  // 测试普通请求（只能通过概率采样）
  let normal_attrs = Attributes::new()
  Attributes::set(normal_attrs, "service.name", StringValue("normal-service"))
  
  let mut normal_sampled_count = 0
  for i in 0..<100 {
    let trace_id = "normal_trace_" + i.to_string()
    if Sampler::should_sample(or_sampler, trace_id, Some(normal_attrs)) {
      normal_sampled_count = normal_sampled_count + 1
    }
  }
  
  // 普通请求应该只有约10%被采样
  let normal_rate = normal_sampled_count.to_float() / 100.0
  assert_true(normal_rate > 0.05 && normal_rate < 0.15)
}

// Test 6: 采样性能测试
test "sampling performance benchmarks" {
  // 创建不同类型的采样器
  let probability_sampler = ProbabilitySampler::new(0.1)
  let adaptive_sampler = AdaptiveSampler::new(0.1, 0.01, 1.0, 100, 1000)
  let attribute_sampler = AttributeSampler::new()
  AttributeSampler::add_rule(attribute_sampler, "error", true, 1.0)
  
  let rate_limiter = RateLimitingSampler::new(1000, 1.0)
  
  // 准备测试数据
  let trace_ids = []
  let attr_sets = []
  
  for i in 0..<10000 {
    trace_ids = trace_ids.push("perf_trace_" + i.to_string())
    
    let attrs = Attributes::new()
    if i % 10 == 0 {
      Attributes::set(attrs, "error", BoolValue(true))
    } else {
      Attributes::set(attrs, "service.name", StringValue("service_" + (i % 5).to_string()))
    }
    
    attr_sets = attr_sets.push(attrs)
  }
  
  // 测试概率采样性能
  let prob_start = Time::now()
  for i in 0..<trace_ids.length() {
    Sampler::should_sample(probability_sampler, trace_ids[i])
  }
  let prob_time = Time::now() - prob_start
  
  // 测试自适应采样性能
  let adaptive_start = Time::now()
  for i in 0..<trace_ids.length() {
    Sampler::should_sample(adaptive_sampler, trace_ids[i])
  }
  let adaptive_time = Time::now() - adaptive_start
  
  // 测试属性采样性能
  let attr_start = Time::now()
  for i in 0..<trace_ids.length() {
    Sampler::should_sample(attribute_sampler, trace_ids[i], Some(attr_sets[i]))
  }
  let attr_time = Time::now() - attr_start
  
  // 测试限流采样性能
  let rate_start = Time::now()
  for i in 0..<trace_ids.length() {
    Sampler::should_sample(rate_limiter, trace_ids[i])
  }
  let rate_time = Time::now() - rate_start
  
  // 验证性能（所有采样器都应该在合理时间内完成）
  assert_true(prob_time < 1000) // 小于1秒
  assert_true(adaptive_time < 1000) // 小于1秒
  assert_true(attr_time < 2000) // 属性采样较慢，小于2秒
  assert_true(rate_time < 1000) // 小于1秒
  
  // 验证采样结果
  let prob_metrics = ProbabilitySampler::get_metrics(probability_sampler)
  let adaptive_metrics = AdaptiveSampler::get_metrics(adaptive_sampler)
  let attr_metrics = AttributeSampler::get_metrics(attribute_sampler)
  let rate_metrics = RateLimitingSampler::get_metrics(rate_limiter)
  
  assert_eq(prob_metrics.total_decisions, 10000)
  assert_eq(adaptive_metrics.total_decisions, 10000)
  assert_eq(attr_metrics.total_decisions, 10000)
  assert_eq(rate_metrics.total_decisions, 10000)
}

// Test 7: 采样一致性测试
test "sampling consistency" {
  // 创建概率采样器
  let sampler = ProbabilitySampler::new(0.1)
  
  // 测试相同trace_id的一致性
  let trace_id = "consistency_test_trace"
  
  let decision1 = Sampler::should_sample(sampler, trace_id)
  let decision2 = Sampler::should_sample(sampler, trace_id)
  let decision3 = Sampler::should_sample(sampler, trace_id)
  
  // 相同的trace_id应该得到相同的决策
  assert_eq(decision1, decision2)
  assert_eq(decision2, decision3)
  
  // 测试不同采样器的一致性
  let sampler2 = ProbabilitySampler::new(0.1)
  
  let decision4 = Sampler::should_sample(sampler2, trace_id)
  
  // 相同配置的不同采样器应该得到相同的决策
  assert_eq(decision1, decision4)
  
  // 测试分布式采样一致性
  let distributed_sampler = DistributedProbabilitySampler::new(0.1)
  
  // 模拟多个节点
  let node_decisions = []
  for node_id in 0..<10 {
    let decision = DistributedSampler::should_sample(distributed_sampler, trace_id, "node_" + node_id.to_string())
    node_decisions = node_decisions.push(decision)
  }
  
  // 所有节点应该得到相同的决策
  for decision in node_decisions {
    assert_eq(decision, decision1)
  }
  
  // 测试父子跨度采样一致性
  let parent_trace_id = "parent_trace"
  let child_trace_id = "child_trace"
  
  let parent_decision = Sampler::should_sample(sampler, parent_trace_id)
  
  // 如果父跨度被采样，子跨度也应该被采样
  let child_decision = Sampler::should_sample_child(sampler, child_trace_id, parent_trace_id, parent_decision)
  
  if parent_decision {
    assert_true(child_decision)
  }
}

// Test 8: 采样监控和指标测试
test "sampling monitoring and metrics" {
  // 创建带监控的采样器
  let monitored_sampler = MonitoredSampler::new(ProbabilitySampler::new(0.1))
  
  // 执行采样操作
  let mut sampled_count = 0
  for i in 0..<1000 {
    let trace_id = "metric_trace_" + i.to_string()
    if Sampler::should_sample(monitored_sampler, trace_id) {
      sampled_count = sampled_count + 1
    }
  }
  
  // 获取采样指标
  let metrics = MonitoredSampler::get_metrics(monitored_sampler)
  
  // 验证指标
  assert_eq(metrics.total_decisions, 1000)
  assert_eq(metrics.sampled_traces, sampled_count)
  assert_eq(metrics.dropped_traces, 1000 - sampled_count)
  
  let actual_rate = metrics.sampled_traces.to_float() / metrics.total_decisions.to_float()
  assert_true(actual_rate > 0.05 && actual_rate < 0.15)
  
  // 测试按属性分组的指标
  let attr_sampler = MonitoredSampler::new(AttributeSampler::new())
  AttributeSampler::add_rule(attr_sampler.sampler, "error", true, 1.0)
  AttributeSampler::add_rule(attr_sampler.sampler, "service.name", "test-service", 0.1)
  
  // 执行带属性的采样
  for i in 0..<1000 {
    let trace_id = "attr_metric_trace_" + i.to_string()
    let attrs = Attributes::new()
    
    if i % 10 == 0 {
      Attributes::set(attrs, "error", BoolValue(true))
    } else {
      Attributes::set(attrs, "service.name", StringValue("test-service"))
    }
    
    Sampler::should_sample(attr_sampler, trace_id, Some(attrs))
  }
  
  // 获取按属性分组的指标
  let attr_metrics = MonitoredSampler::get_metrics_by_attribute(attr_sampler, "error")
  
  match attr_metrics.get(true) {
    Some(error_metrics) => {
      assert_eq(error_metrics.total_decisions, 100) // 10%的错误请求
      assert_eq(error_metrics.sampled_traces, 100) // 错误请求100%采样
    }
    None => assert_true(false)
  }
  
  let service_metrics = MonitoredSampler::get_metrics_by_attribute(attr_sampler, "service.name")
  
  match service_metrics.get("test-service") {
    Some(service_metric) => {
      assert_eq(service_metric.total_decisions, 900) // 90%的普通请求
      assert_true(service_metric.sampled_traces > 80 && service_metric.sampled_traces < 100) // 约10%采样
    }
    None => assert_true(false)
  }
  
  // 测试实时采样统计
  let real_time_stats = MonitoredSampler::get_real_time_stats(monitored_sampler)
  assert_true(real_time_stats.decisions_per_second > 0)
  assert_true(real_time_stats.current_sample_rate >= 0.0 && real_time_stats.current_sample_rate <= 1.0)
  assert_true(real_time_stats.efficiency >= 0.0 && real_time_stats.efficiency <= 1.0)
}