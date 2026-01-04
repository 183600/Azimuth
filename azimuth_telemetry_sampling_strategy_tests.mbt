// Azimuth 遥测数据采样策略测试用例
// 专注于测试不同的采样策略和算法

// 测试1: 固定比例采样策略
test "固定比例采样策略测试" {
  // 创建固定比例采样器
  let sampler = FixedRateSampler::new(0.1)  // 10% 采样率
  
  // 测试采样决策
  let decision1 = Sampler::should_sample(sampler, "trace-001", "span-001", "operation.name")
  let decision2 = Sampler::should_sample(sampler, "trace-002", "span-002", "operation.name")
  let decision3 = Sampler::should_sample(sampler, "trace-003", "span-003", "operation.name")
  
  // 验证采样决策
  match decision1 {
    Decision::RecordAndSample => assert_true(true)
    Decision::Drop => assert_true(true)  // 两种决策都可能，取决于实现
    Decision::RecordOnly => assert_true(false)  // 固定采样器不应该返回此决策
  }
  
  // 验证采样器属性
  let attributes = Sampler::get_attributes(sampler)
  assert_eq(attributes.get("sampler.type"), Some("fixed_rate"))
  assert_eq(attributes.get("sampler.rate"), Some("0.1"))
}

// 测试2: 基于概率的采样策略
test "基于概率的采样策略测试" {
  // 创建概率采样器
  let sampler = ProbabilitySampler::new(0.25)  // 25% 采样率
  
  // 测试多个采样决策
  let decisions = []
  for i in 1..=100 {
    let trace_id = "trace-" + i.to_string()
    let span_id = "span-" + i.to_string()
    let decision = Sampler::should_sample(sampler, trace_id, span_id, "test.operation")
    decisions = decisions.push(decision)
  }
  
  // 统计采样结果
  let mut sampled_count = 0
  let mut dropped_count = 0
  
  for decision in decisions {
    match decision {
      Decision::RecordAndSample => sampled_count = sampled_count + 1
      Decision::Drop => dropped_count = dropped_count + 1
      Decision::RecordOnly => assert_true(false)  // 概率采样器不应该返回此决策
    }
  }
  
  // 验证采样率在合理范围内 (20% - 30%)
  let actual_rate = sampled_count.to_float() / 100.0
  assert_true(actual_rate >= 0.2 and actual_rate <= 0.3)
  
  // 验证采样器属性
  let attributes = Sampler::get_attributes(sampler)
  assert_eq(attributes.get("sampler.type"), Some("probability"))
  assert_eq(attributes.get("sampler.rate"), Some("0.25"))
}

// 测试3: 基于父级的采样策略
test "基于父级的采样策略测试" {
  // 创建父级感知采样器
  let sampler = ParentBasedSampler::new(ProbabilitySampler::new(0.5))
  
  // 测试有父span且父span被采样的情况
  let parent_context = SpanContext::new("parent-trace", "parent-span", true, "")
  let decision_with_sampled_parent = Sampler::should_sample_with_parent(
    sampler, 
    "child-trace", 
    "child-span", 
    "child.operation", 
    Some(parent_context)
  )
  
  // 子span应该跟随父span的采样决策
  match decision_with_sampled_parent {
    Decision::RecordAndSample => assert_true(true)
    Decision::Drop => assert_true(false)
    Decision::RecordOnly => assert_true(false)
  }
  
  // 测试有父span但父span未被采样的情况
  let unsampled_parent_context = SpanContext::new("parent-trace", "parent-span", false, "")
  let decision_with_unsampled_parent = Sampler::should_sample_with_parent(
    sampler, 
    "child-trace", 
    "child-span", 
    "child.operation", 
    Some(unsampled_parent_context)
  )
  
  // 子span应该跟随父span的采样决策
  match decision_with_unsampled_parent {
    Decision::RecordAndSample => assert_true(false)
    Decision::Drop => assert_true(true)
    Decision::RecordOnly => assert_true(false)
  }
  
  // 测试无父span的情况
  let decision_without_parent = Sampler::should_sample_with_parent(
    sampler, 
    "root-trace", 
    "root-span", 
    "root.operation", 
    None
  )
  
  // 无父span时应该使用根采样器
  match decision_without_parent {
    Decision::RecordAndSample => assert_true(true)  // 可能被采样
    Decision::Drop => assert_true(true)  // 也可能不被采样
    Decision::RecordOnly => assert_true(false)  // 根采样器不应该返回此决策
  }
}

// 测试4: 基于属性的采样策略
test "基于属性的采样策略测试" {
  // 创建属性采样器
  let sampler = AttributeBasedSampler::new([
    ("service.name", "critical.service"),
    ("operation.type", "database.query"),
    ("user.premium", "true")
  ])
  
  // 测试匹配所有属性的情况
  let matching_attributes = [
    ("service.name", StringValue("critical.service")),
    ("operation.type", StringValue("database.query")),
    ("user.premium", StringValue("true")),
    ("extra.attr", StringValue("extra.value"))
  ]
  
  let decision_matching = Sampler::should_sample_with_attributes(
    sampler, 
    "trace-001", 
    "span-001", 
    "matched.operation", 
    matching_attributes
  )
  
  // 匹配所有关键属性应该被采样
  match decision_matching {
    Decision::RecordAndSample => assert_true(true)
    Decision::Drop => assert_true(false)
    Decision::RecordOnly => assert_true(false)
  }
  
  // 测试部分匹配属性的情况
  let partial_attributes = [
    ("service.name", StringValue("critical.service")),
    ("operation.type", StringValue("http.request")),
    ("user.premium", StringValue("false"))
  ]
  
  let decision_partial = Sampler::should_sample_with_attributes(
    sampler, 
    "trace-002", 
    "span-002", 
    "partial.operation", 
    partial_attributes
  )
  
  // 部分匹配不应该被采样
  match decision_partial {
    Decision::RecordAndSample => assert_true(false)
    Decision::Drop => assert_true(true)
    Decision::RecordOnly => assert_true(false)
  }
  
  // 测试不匹配任何属性的情况
  let no_match_attributes = [
    ("service.name", StringValue("regular.service")),
    ("operation.type", StringValue("http.request"))
  ]
  
  let decision_no_match = Sampler::should_sample_with_attributes(
    sampler, 
    "trace-003", 
    "span-003", 
    "no.match.operation", 
    no_match_attributes
  )
  
  // 不匹配任何属性不应该被采样
  match decision_no_match {
    Decision::RecordAndSample => assert_true(false)
    Decision::Drop => assert_true(true)
    Decision::RecordOnly => assert_true(false)
  }
}

// 测试5: 自适应采样策略
test "自适应采样策略测试" {
  // 创建自适应采样器
  let sampler = AdaptiveSampler::new(0.1, 1000, 10000)  // 初始10%，最小1000，最大10000
  
  // 模拟低流量情况下的采样
  let low_traffic_decisions = []
  for i in 1..=100 {
    let trace_id = "low-traffic-" + i.to_string()
    let decision = Sampler::should_sample(sampler, trace_id, "span-" + i.to_string(), "low.traffic.operation")
    low_traffic_decisions = low_traffic_decisions.push(decision)
  }
  
  // 统计低流量采样结果
  let mut low_traffic_sampled = 0
  for decision in low_traffic_decisions {
    match decision {
      Decision::RecordAndSample => low_traffic_sampled = low_traffic_sampled + 1
      _ => {}
    }
  }
  
  // 低流量时应该提高采样率
  let low_traffic_rate = low_traffic_sampled.to_float() / 100.0
  assert_true(low_traffic_rate >= 0.1)  // 应该大于等于初始采样率
  
  // 模拟高流量情况下的采样
  let high_traffic_decisions = []
  for i in 1..=10000 {
    let trace_id = "high-traffic-" + i.to_string()
    let decision = Sampler::should_sample(sampler, trace_id, "span-" + i.to_string(), "high.traffic.operation")
    high_traffic_decisions = high_traffic_decisions.push(decision)
  }
  
  // 统计高流量采样结果
  let mut high_traffic_sampled = 0
  for decision in high_traffic_decisions {
    match decision {
      Decision::RecordAndSample => high_traffic_sampled = high_traffic_sampled + 1
      _ => {}
    }
  }
  
  // 高流量时应该降低采样率
  let high_traffic_rate = high_traffic_sampled.to_float() / 10000.0
  assert_true(high_traffic_rate <= 0.1)  // 应该小于等于初始采样率
}

// 测试6: 采样器组合策略
test "采样器组合策略测试" {
  // 创建多个采样器
  let sampler1 = FixedRateSampler::new(0.2)  // 20% 固定采样
  let sampler2 = AttributeBasedSampler::new([("service.critical", "true")])  // 基于属性采样
  let sampler3 = ProbabilitySampler::new(0.3)  // 30% 概率采样
  
  // 创建组合采样器 (OR 逻辑)
  let or_sampler = CompositeSampler::or([sampler1, sampler2, sampler3])
  
  // 测试不满足任何条件的情况
  let decision_none = Sampler::should_sample_with_attributes(
    or_sampler, 
    "trace-001", 
    "span-001", 
    "regular.operation", 
    [("service.critical", StringValue("false"))]
  )
  
  // OR逻辑：至少一个采样器同意就采样
  match decision_none {
    Decision::RecordAndSample => assert_true(true)  // 可能被概率采样器采样
    Decision::Drop => assert_true(true)  // 也可能所有采样器都拒绝
    Decision::RecordOnly => assert_true(false)
  }
  
  // 测试满足属性条件的情况
  let decision_attr = Sampler::should_sample_with_attributes(
    or_sampler, 
    "trace-002", 
    "span-002", 
    "critical.operation", 
    [("service.critical", StringValue("true"))]
  )
  
  // OR逻辑：满足属性条件应该被采样
  match decision_attr {
    Decision::RecordAndSample => assert_true(true)
    Decision::Drop => assert_true(false)
    Decision::RecordOnly => assert_true(false)
  }
  
  // 创建组合采样器 (AND 逻辑)
  let and_sampler = CompositeSampler::and([sampler1, sampler2])
  
  // 测试满足所有条件的情况
  let decision_all = Sampler::should_sample_with_attributes(
    and_sampler, 
    "trace-003", 
    "span-003", 
    "all.conditions.operation", 
    [("service.critical", StringValue("true"))]
  )
  
  // AND逻辑：所有采样器同意才采样
  match decision_all {
    Decision::RecordAndSample => assert_true(true)  // 可能被采样
    Decision::Drop => assert_true(true)  // 也可能不被采样
    Decision::RecordOnly => assert_true(false)
  }
  
  // 测试不满足所有条件的情况
  let decision_not_all = Sampler::should_sample_with_attributes(
    and_sampler, 
    "trace-004", 
    "span-004", 
    "partial.conditions.operation", 
    [("service.critical", StringValue("false"))]
  )
  
  // AND逻辑：不满足所有条件不应该被采样
  match decision_not_all {
    Decision::RecordAndSample => assert_true(false)
    Decision::Drop => assert_true(true)
    Decision::RecordOnly => assert_true(false)
  }
}

// 测试7: 采样器动态配置
test "采样器动态配置测试" {
  // 创建可配置采样器
  let sampler = ConfigurableSampler::new(FixedRateSampler::new(0.1))
  
  // 验证初始配置
  let initial_decision = Sampler::should_sample(sampler, "trace-001", "span-001", "test.operation")
  match initial_decision {
    Decision::RecordAndSample => assert_true(true)
    Decision::Drop => assert_true(true)
    Decision::RecordOnly => assert_true(false)
  }
  
  // 动态更新采样器配置
  let new_sampler = ProbabilitySampler::new(0.5)
  ConfigurableSampler::update(sampler, new_sampler)
  
  // 验证更新后的配置
  let updated_decision = Sampler::should_sample(sampler, "trace-002", "span-002", "test.operation")
  match updated_decision {
    Decision::RecordAndSample => assert_true(true)
    Decision::Drop => assert_true(true)
    Decision::RecordOnly => assert_true(false)
  }
  
  // 验证配置历史
  let config_history = ConfigurableSampler::get_config_history(sampler)
  assert_eq(config_history.length(), 2)  // 初始配置 + 更新配置
  
  // 验证当前配置类型
  let current_config = ConfigurableSampler::get_current_config(sampler)
  assert_eq(current_config.get("sampler.type"), Some("probability"))
}

// 测试8: 采样器性能测试
test "采样器性能测试" {
  // 创建不同类型的采样器
  let fixed_sampler = FixedRateSampler::new(0.1)
  let prob_sampler = ProbabilitySampler::new(0.1)
  let attr_sampler = AttributeBasedSampler::new([("service.critical", "true")])
  
  // 测试固定采样器性能
  let start_time = Clock::now_unix_nanos(Clock::system())
  for i in 1..=10000 {
    let trace_id = "perf-fixed-" + i.to_string()
    Sampler::should_sample(fixed_sampler, trace_id, "span-" + i.to_string(), "perf.operation")
  }
  let end_time = Clock::now_unix_nanos(Clock::system())
  let fixed_duration = end_time - start_time
  
  // 测试概率采样器性能
  let start_time = Clock::now_unix_nanos(Clock::system())
  for i in 1..=10000 {
    let trace_id = "perf-prob-" + i.to_string()
    Sampler::should_sample(prob_sampler, trace_id, "span-" + i.to_string(), "perf.operation")
  }
  let end_time = Clock::now_unix_nanos(Clock::system())
  let prob_duration = end_time - start_time
  
  // 测试属性采样器性能
  let start_time = Clock::now_unix_nanos(Clock::system())
  for i in 1..=10000 {
    let trace_id = "perf-attr-" + i.to_string()
    Sampler::should_sample_with_attributes(
      attr_sampler, 
      trace_id, 
      "span-" + i.to_string(), 
      "perf.operation", 
      [("service.critical", StringValue("true"))]
    )
  }
  let end_time = Clock::now_unix_nanos(Clock::system())
  let attr_duration = end_time - start_time
  
  // 验证性能在合理范围内
  assert_true(fixed_duration < 1000000000L)  // 小于1秒
  assert_true(prob_duration < 1000000000L)   // 小于1秒
  assert_true(attr_duration < 2000000000L)   // 属性采样可能稍慢，但仍应小于2秒
  
  // 验证相对性能
  assert_true(fixed_duration <= prob_duration)  // 固定采样应该比概率采样快
  assert_true(prob_duration <= attr_duration)   // 概率采样应该比属性采样快
}

// 测试9: 采样器错误处理
test "采样器错误处理测试" {
  // 创建会抛出异常的采样器
  let sampler = FaultySampler::new()
  
  // 测试采样器异常处理
  let decision = Sampler::should_sample(sampler, "trace-001", "span-001", "faulty.operation")
  
  // 异常情况下应该返回默认决策
  match decision {
    Decision::RecordAndSample => assert_true(false)
    Decision::Drop => assert_true(true)  // 默认应该丢弃
    Decision::RecordOnly => assert_true(false)
  }
  
  // 验证错误计数
  let error_count = FaultySampler::get_error_count(sampler)
  assert_true(error_count > 0)
  
  // 测试采样器恢复
  FaultySampler::recover(sampler)
  let recovered_decision = Sampler::should_sample(sampler, "trace-002", "span-002", "recovered.operation")
  
  // 恢复后应该正常工作
  match recovered_decision {
    Decision::RecordAndSample => assert_true(true)
    Decision::Drop => assert_true(true)
    Decision::RecordOnly => assert_true(false)
  }
}

// 测试10: 采样器边界条件测试
test "采样器边界条件测试" {
  // 测试极端采样率
  let always_sampler = FixedRateSampler::new(1.0)  // 100% 采样
  let never_sampler = FixedRateSampler::new(0.0)   // 0% 采样
  
  // 测试总是采样
  for i in 1..=100 {
    let decision = Sampler::should_sample(always_sampler, "always-trace", "always-span", "always.operation")
    match decision {
      Decision::RecordAndSample => assert_true(true)
      _ => assert_true(false)
    }
  }
  
  // 测试从不采样
  for i in 1..=100 {
    let decision = Sampler::should_sample(never_sampler, "never-trace", "never-span", "never.operation")
    match decision {
      Decision::Drop => assert_true(true)
      _ => assert_true(false)
    }
  }
  
  // 测试空字符串参数
  let empty_decision = Sampler::should_sample(always_sampler, "", "", "")
  match empty_decision {
    Decision::RecordAndSample => assert_true(true)
    _ => assert_true(false)
  }
  
  // 测试极长字符串参数
  let long_trace_id = "a".repeat(1000)
  let long_span_id = "b".repeat(1000)
  let long_operation = "c".repeat(1000)
  let long_decision = Sampler::should_sample(always_sampler, long_trace_id, long_span_id, long_operation)
  match long_decision {
    Decision::RecordAndSample => assert_true(true)
    _ => assert_true(false)
  }
  
  // 测试特殊字符
  let special_chars = "!@#$%^&*()_+-=[]{}|;':\",./<>?"
  let special_decision = Sampler::should_sample(always_sampler, special_chars, special_chars, special_chars)
  match special_decision {
    Decision::RecordAndSample => assert_true(true)
    _ => assert_true(false)
  }
}