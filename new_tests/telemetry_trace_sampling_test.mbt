// 遥测链路追踪采样测试用例

test "always_on_sampling_strategy" {
  // 测试始终开启采样策略
  
  let sampler = azimuth::telemetry::api::trace::sampler::AlwaysOnSampler::new()
  
  // 创建采样上下文
  let context = azimuth::telemetry::api::trace::SamplingContext::empty()
    .with("trace.id", "trace-12345")
    .with("span.name", "test-span")
  
  // 测试采样决策
  let decision = sampler.should_sample(context)
  assert_eq(decision.is_sampled(), true)
  assert_eq(decision.get_decision(), "RECORD_AND_SAMPLE")
  
  // 验证采样属性
  let attributes = decision.get_attributes()
  assert_eq(attributes.size(), 0)  // AlwaysOn通常不添加额外属性
}

test "always_off_sampling_strategy" {
  // 测试始终关闭采样策略
  
  let sampler = azimuth::telemetry::api::trace::sampler::AlwaysOffSampler::new()
  
  // 创建采样上下文
  let context = azimuth::telemetry::api::trace::SamplingContext::empty()
    .with("trace.id", "trace-67890")
    .with("span.name", "another-test-span")
  
  // 测试采样决策
  let decision = sampler.should_sample(context)
  assert_eq(decision.is_sampled(), false)
  assert_eq(decision.get_decision(), "DROP")
  
  // 验证采样属性
  let attributes = decision.get_attributes()
  assert_eq(attributes.size(), 0)
}

test "trace_id_ratio_based_sampling" {
  // 测试基于Trace ID比例的采样
  
  // 创建50%采样率的采样器
  let sampler = azimuth::telemetry::api::trace::sampler::TraceIdRatioBasedSampler::new(0.5)
  
  // 测试多个不同的trace ID
  let trace_ids = [
    "00000000000000000000000000000000",  // 应该被采样
    "7fffffffffffffffffffffffffffffff",  // 应该被采样
    "80000000000000000000000000000001",  // 不应该被采样
    "ffffffffffffffffffffffffffffffff"   // 不应该被采样
  ]
  
  let expected_results = [true, true, false, false]
  
  for i in range(0, trace_ids.length()) {
    let context = azimuth::telemetry::api::trace::SamplingContext::empty()
      .with("trace.id", trace_ids[i])
      .with("span.name", "ratio-test-span")
    
    let decision = sampler.should_sample(context)
    assert_eq(decision.is_sampled(), expected_results[i])
  }
}

test "parent_based_sampling_strategy" {
  // 测试基于父级的采样策略
  
  // 创建父级采样器
  let parent_sampler = azimuth::telemetry::api::trace::sampler::ParentBasedSampler::new(
    azimuth::telemetry::api::trace::sampler::AlwaysOnSampler::new()
  )
  
  // 测试有父级且父级被采样的情况
  let context_with_sampled_parent = azimuth::telemetry::api::trace::SamplingContext::empty()
    .with("trace.id", "trace-with-sampled-parent")
    .with("span.name", "child-span")
    .with("parent.is_sampled", "true")
  
  let decision1 = parent_sampler.should_sample(context_with_sampled_parent)
  assert_eq(decision1.is_sampled(), true)
  
  // 测试有父级但父级未被采样的情况
  let context_with_unsampled_parent = azimuth::telemetry::api::trace::SamplingContext::empty()
    .with("trace.id", "trace-with-unsampled-parent")
    .with("span.name", "child-span")
    .with("parent.is_sampled", "false")
  
  let decision2 = parent_sampler.should_sample(context_with_unsampled_parent)
  assert_eq(decision2.is_sampled(), false)
  
  // 测试无父级的情况（应该使用根采样器）
  let context_without_parent = azimuth::telemetry::api::trace::SamplingContext::empty()
    .with("trace.id", "trace-without-parent")
    .with("span.name", "root-span")
  
  let decision3 = parent_sampler.should_sample(context_without_parent)
  assert_eq(decision3.is_sampled(), true)  // 使用AlwaysOn根采样器
}

test "attribute_based_sampling_strategy" {
  // 测试基于属性的采样策略
  
  // 创建基于属性的采样器
  let sampler = azimuth::telemetry::api::trace::sampler::AttributeBasedSampler::new(
    "http.status_code",  // 属性名
    ["500", "502", "503"],  // 触发采样的值
    azimuth::telemetry::api::trace::sampler::AlwaysOffSampler::new()  // 默认采样器
  )
  
  // 测试匹配属性值的情况
  let context_error = azimuth::telemetry::api::trace::SamplingContext::empty()
    .with("trace.id", "trace-error")
    .with("span.name", "error-span")
    .with("http.status_code", "500")
  
  let decision1 = sampler.should_sample(context_error)
  assert_eq(decision1.is_sampled(), true)
  
  // 测试不匹配属性值的情况
  let context_success = azimuth::telemetry::api::trace::SamplingContext::empty()
    .with("trace.id", "trace-success")
    .with("span.name", "success-span")
    .with("http.status_code", "200")
  
  let decision2 = sampler.should_sample(context_success)
  assert_eq(decision2.is_sampled(), false)  // 使用默认采样器
}

test "composite_sampling_strategy" {
  // 测试复合采样策略
  
  // 创建多个采样器
  let sampler1 = azimuth::telemetry::api::trace::sampler::TraceIdRatioBasedSampler::new(0.3)
  let sampler2 = azimuth::telemetry::api::trace::sampler::AttributeBasedSampler::new(
    "service.name",
    ["critical-service"],
    azimuth::telemetry::api::trace::sampler::AlwaysOffSampler::new()
  )
  
  // 创建复合采样器（任一匹配即采样）
  let composite_sampler = azimuth::telemetry::api::trace::sampler::CompositeSampler::any_of([sampler1, sampler2])
  
  // 测试关键服务（应该总是被采样）
  let context_critical = azimuth::telemetry::api::trace::SamplingContext::empty()
    .with("trace.id", "trace-critical")
    .with("span.name", "critical-span")
    .with("service.name", "critical-service")
  
  let decision1 = composite_sampler.should_sample(context_critical)
  assert_eq(decision1.is_sampled(), true)
  
  // 测试普通服务（可能被采样，取决于trace ID）
  let context_normal = azimuth::telemetry::api::trace::SamplingContext::empty()
    .with("trace.id", "00000000000000000000000000000001")
    .with("span.name", "normal-span")
    .with("service.name", "normal-service")
  
  let decision2 = composite_sampler.should_sample(context_normal)
  // 这个trace ID应该被采样器1采样
  assert_eq(decision2.is_sampled(), true)
}

test "adaptive_sampling_strategy" {
  // 测试自适应采样策略
  
  // 创建自适应采样器
  let adaptive_sampler = azimuth::telemetry::api::trace::sampler::AdaptiveSampler::new(
    10,   // 目标每秒采样数
    100   // 采样率调整周期（秒）
  )
  
  // 模拟低流量情况
  for i in range(0, 5) {
    let context = azimuth::telemetry::api::trace::SamplingContext::empty()
      .with("trace.id", "trace-" + i.to_string())
      .with("span.name", "low-traffic-span")
    
    let decision = adaptive_sampler.should_sample(context)
    assert_eq(decision.is_sampled(), true)  // 低流量时应该采样所有请求
  }
  
  // 模拟高流量情况
  let sampled_count = 0
  for i in range(0, 100) {
    let context = azimuth::telemetry::api::trace::SamplingContext::empty()
      .with("trace.id", "trace-high-" + i.to_string())
      .with("span.name", "high-traffic-span")
    
    let decision = adaptive_sampler.should_sample(context)
    if decision.is_sampled() {
      sampled_count = sampled_count + 1
    }
  }
  
  // 高流量时应该只采样一部分请求
  assert_eq(sampled_count > 0, true)
  assert_eq(sampled_count < 100, true)
}

test "sampling_decision_attributes" {
  // 测试采样决策属性
  
  let sampler = azimuth::telemetry::api::trace::sampler::TraceIdRatioBasedSampler::new(0.1)
  
  let context = azimuth::telemetry::api::trace::SamplingContext::empty()
    .with("trace.id", "00000000000000000000000000000001")
    .with("span.name", "attributes-test-span")
    .with("service.name", "test-service")
    .with("environment", "production")
  
  let decision = sampler.should_sample(context)
  
  // 验证采样决策包含的属性
  let attributes = decision.get_attributes()
  assert_eq(attributes.size() > 0, true)
  
  // 检查是否包含采样率信息
  assert_eq(attributes.get("sampler.type").unwrap().to_string(), "TraceIdRatioBased")
  assert_eq(attributes.get("sampler.probability").unwrap().to_string(), "0.1")
}

test "sampling_edge_cases_and_validation" {
  // 测试采样的边界情况和验证
  
  // 测试极端采样率
  let zero_sampler = azimuth::telemetry::api::trace::sampler::TraceIdRatioBasedSampler::new(0.0)
  let one_sampler = azimuth::telemetry::api::trace::sampler::TraceIdRatioBasedSampler::new(1.0)
  
  let context = azimuth::telemetry::api::trace::SamplingContext::empty()
    .with("trace.id", "00000000000000000000000000000001")
    .with("span.name", "edge-case-span")
  
  // 0%采样率应该总是不采样
  let decision1 = zero_sampler.should_sample(context)
  assert_eq(decision1.is_sampled(), false)
  
  // 100%采样率应该总是采样
  let decision2 = one_sampler.should_sample(context)
  assert_eq(decision2.is_sampled(), true)
  
  // 测试无效采样率（应该被限制在[0, 1]范围内）
  let invalid_sampler1 = azimuth::telemetry::api::trace::sampler::TraceIdRatioBasedSampler::new(-0.5)
  let invalid_sampler2 = azimuth::telemetry::api::trace::sampler::TraceIdRatioBasedSampler::new(1.5)
  
  let decision3 = invalid_sampler1.should_sample(context)
  let decision4 = invalid_sampler2.should_sample(context)
  
  // 负采样率应该被当作0
  assert_eq(decision3.is_sampled(), false)
  // 大于1的采样率应该被当作1
  assert_eq(decision4.is_sampled(), true)
  
  // 测试空上下文
  let empty_context = azimuth::telemetry::api::trace::SamplingContext::empty()
  let decision5 = one_sampler.should_sample(empty_context)
  assert_eq(decision5.is_sampled(), true)
}

test "sampling_performance_and_concurrency" {
  // 测试采样性能和并发
  
  let sampler = azimuth::telemetry::api::trace::sampler::TraceIdRatioBasedSampler::new(0.1)
  
  // 测试大量采样的性能
  let start_time = azimuth::telemetry::api::common::time::now()
  let sampled_count = 0
  
  for i in range(0, 10000) {
    let context = azimuth::telemetry::api::trace::SamplingContext::empty()
      .with("trace.id", "trace-perf-" + i.to_string())
      .with("span.name", "performance-span")
    
    let decision = sampler.should_sample(context)
    if decision.is_sampled() {
      sampled_count = sampled_count + 1
    }
  }
  
  let end_time = azimuth::telemetry::api::common::time::now()
  let duration = end_time - start_time
  
  // 验证采样率大约为10%
  let actual_rate = sampled_count.to_float() / 10000.0
  assert_eq(actual_rate > 0.08 && actual_rate < 0.12, true)
  
  // 验证性能（应该在合理时间内完成）
  assert_eq(duration < 1000000, true)  // 小于1秒（微秒单位）
}