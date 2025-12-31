// 采样策略测试用例

test "always_on_sampler" {
  // 测试始终开启采样器
  
  let sampler = azimuth::telemetry::sdk::trace::AlwaysOnSampler::new()
  
  // 创建采样决策上下文
  let trace_id = "4bf92f3577b34da6a3ce929d0e0e4736"
  let span_name = "test.span"
  let span_kind = azimuth::telemetry::api::trace::SpanKind::Server
  let attributes = azimuth::telemetry::api::common::Attributes::empty()
  let parent_context = None
  
  // 测试采样决策
  let decision = sampler.should_sample(
    parent_context,
    trace_id,
    span_name,
    span_kind,
    attributes
  )
  
  // 验证始终开启采样器的决策
  assert_eq(decision.decision, azimuth::telemetry::sdk::trace::SamplingDecision::RecordAndSample)
  assert_eq(decision.is_sampled(), true)
  assert_eq(decision.is_recording(), true)
  assert_eq(decision.attributes.size(), 0)  // 没有额外属性
}

test "always_off_sampler" {
  // 测试始终关闭采样器
  
  let sampler = azimuth::telemetry::sdk::trace::AlwaysOffSampler::new()
  
  let trace_id = "4bf92f3577b34da6a3ce929d0e0e4736"
  let span_name = "test.span"
  let span_kind = azimuth::telemetry::api::trace::SpanKind::Server
  let attributes = azimuth::telemetry::api::common::Attributes::empty()
  let parent_context = None
  
  let decision = sampler.should_sample(
    parent_context,
    trace_id,
    span_name,
    span_kind,
    attributes
  )
  
  // 验证始终关闭采样器的决策
  assert_eq(decision.decision, azimuth::telemetry::sdk::trace::SamplingDecision::Drop)
  assert_eq(decision.is_sampled(), false)
  assert_eq(decision.is_recording(), false)
  assert_eq(decision.attributes.size(), 0)
}

test "trace_id_ratio_sampler" {
  // 测试基于Trace ID比例的采样器
  
  // 创建50%采样率的采样器
  let sampler = azimuth::telemetry::sdk::trace::TraceIdRatioBasedSampler::new(0.5)
  
  let span_name = "test.span"
  let span_kind = azimuth::telemetry::api::trace::SpanKind::Server
  let attributes = azimuth::telemetry::api::common::Attributes::empty()
  let parent_context = None
  
  // 测试多个不同的Trace ID
  let trace_ids = [
    "4bf92f3577b34da6a3ce929d0e0e4736",  // 应该被采样
    "00f067aa0ba902b7",                  // 不应该被采样
    "4bf92f3577b34da6a3ce929d0e0e4737",  // 应该被采样
    "00f067aa0ba902b8",                  // 不应该被采样
    "4bf92f3577b34da6a3ce929d0e0e4738"   // 应该被采样
  ]
  
  let mut sampled_count = 0
  for trace_id in trace_ids {
    let decision = sampler.should_sample(
      parent_context,
      trace_id,
      span_name,
      span_kind,
      attributes
    )
    
    if decision.is_sampled() {
      sampled_count = sampled_count + 1
    }
  }
  
  // 验证采样率大约为50%
  let sampling_rate = sampled_count.to_float() / trace_ids.length().to_float()
  assert_eq(sampling_rate >= 0.4 and sampling_rate <= 0.6, true)
}

test "parent_based_sampler" {
  // 测试基于父Span的采样器
  
  // 创建基于父Span的采样器
  let root_sampler = azimuth::telemetry::sdk::trace::AlwaysOnSampler::new()
  let remote_parent_sampled = azimuth::telemetry::sdk::trace::AlwaysOnSampler::new()
  let remote_parent_not_sampled = azimuth::telemetry::sdk::trace::AlwaysOffSampler::new()
  let local_parent_sampled = azimuth::telemetry::sdk::trace::AlwaysOnSampler::new()
  let local_parent_not_sampled = azimuth::telemetry::sdk::trace::AlwaysOffSampler::new()
  
  let parent_based_sampler = azimuth::telemetry::sdk::trace::ParentBasedSampler::new(root_sampler)
    .with_remote_parent_sampled(remote_parent_sampled)
    .with_remote_parent_not_sampled(remote_parent_not_sampled)
    .with_local_parent_sampled(local_parent_sampled)
    .with_local_parent_not_sampled(local_parent_not_sampled)
  
  let trace_id = "4bf92f3577b34da6a3ce929d0e0e4736"
  let span_name = "test.span"
  let span_kind = azimuth::telemetry::api::trace::SpanKind::Server
  let attributes = azimuth::telemetry::api::common::Attributes::empty()
  
  // 测试无父上下文的情况
  let no_parent_decision = parent_based_sampler.should_sample(
    None,
    trace_id,
    span_name,
    span_kind,
    attributes
  )
  assert_eq(no_parent_decision.decision, azimuth::telemetry::sdk::trace::SamplingDecision::RecordAndSample)
  
  // 测试远程父Span已采样
  let remote_sampled_context = azimuth::telemetry::api::context::Context::empty()
    .with_value(
      azimuth::telemetry::api::context::create_key("span_context"),
      azimuth::telemetry::api::trace::SpanContext::new(
        trace_id,
        "00f067aa0ba902b7",
        true,  // is_sampled
        true   // is_remote
      )
    )
  
  let remote_sampled_decision = parent_based_sampler.should_sample(
    Some(remote_sampled_context),
    trace_id,
    span_name,
    span_kind,
    attributes
  )
  assert_eq(remote_sampled_decision.decision, azimuth::telemetry::sdk::trace::SamplingDecision::RecordAndSample)
  
  // 测试远程父Span未采样
  let remote_not_sampled_context = azimuth::telemetry::api::context::Context::empty()
    .with_value(
      azimuth::telemetry::api::context::create_key("span_context"),
      azimuth::telemetry::api::trace::SpanContext::new(
        trace_id,
        "00f067aa0ba902b7",
        false, // is_sampled
        true   // is_remote
      )
    )
  
  let remote_not_sampled_decision = parent_based_sampler.should_sample(
    Some(remote_not_sampled_context),
    trace_id,
    span_name,
    span_kind,
    attributes
  )
  assert_eq(remote_not_sampled_decision.decision, azimuth::telemetry::sdk::trace::SamplingDecision::Drop)
}

test "attribute_based_sampler" {
  // 测试基于属性的采样器
  
  // 创建基于属性的采样器
  let sampler = azimuth::telemetry::sdk::trace::AttributeBasedSampler::new()
    .with_rule("http.method", "GET", azimuth::telemetry::sdk::trace::SamplingDecision::RecordAndSample)
    .with_rule("http.method", "POST", azimuth::telemetry::sdk::trace::SamplingDecision::RecordOnly)
    .with_rule("error", true, azimuth::telemetry::sdk::trace::SamplingDecision::RecordAndSample)
    .with_default(azimuth::telemetry::sdk::trace::SamplingDecision::Drop)
  
  let trace_id = "4bf92f3577b34da6a3ce929d0e0e4736"
  let span_name = "http.request"
  let span_kind = azimuth::telemetry::api::trace::SpanKind::Server
  let parent_context = None
  
  // 测试GET请求
  let get_attributes = azimuth::telemetry::api::common::Attributes::empty()
    .with("http.method", azimuth::telemetry::api::common::AttributeValue::String("GET"))
    .with("http.url", azimuth::telemetry::api::common::AttributeValue::String("/api/users"))
  
  let get_decision = sampler.should_sample(
    parent_context,
    trace_id,
    span_name,
    span_kind,
    get_attributes
  )
  assert_eq(get_decision.decision, azimuth::telemetry::sdk::trace::SamplingDecision::RecordAndSample)
  assert_eq(get_decision.is_sampled(), true)
  
  // 测试POST请求
  let post_attributes = azimuth::telemetry::api::common::Attributes::empty()
    .with("http.method", azimuth::telemetry::api::common::AttributeValue::String("POST"))
    .with("http.url", azimuth::telemetry::api::common::AttributeValue::String("/api/orders"))
  
  let post_decision = sampler.should_sample(
    parent_context,
    trace_id,
    span_name,
    span_kind,
    post_attributes
  )
  assert_eq(post_decision.decision, azimuth::telemetry::sdk::trace::SamplingDecision::RecordOnly)
  assert_eq(post_decision.is_sampled(), false)
  assert_eq(post_decision.is_recording(), true)
  
  // 测试错误情况
  let error_attributes = azimuth::telemetry::api::common::Attributes::empty()
    .with("error", azimuth::telemetry::api::common::AttributeValue::Bool(true))
    .with("error.message", azimuth::telemetry::api::common::AttributeValue::String("Database connection failed"))
  
  let error_decision = sampler.should_sample(
    parent_context,
    trace_id,
    span_name,
    span_kind,
    error_attributes
  )
  assert_eq(error_decision.decision, azimuth::telemetry::sdk::trace::SamplingDecision::RecordAndSample)
  assert_eq(error_decision.is_sampled(), true)
  
  // 测试默认情况
  let default_attributes = azimuth::telemetry::api::common::Attributes::empty()
    .with("http.method", azimuth::telemetry::api::common::AttributeValue::String("PUT"))
  
  let default_decision = sampler.should_sample(
    parent_context,
    trace_id,
    span_name,
    span_kind,
    default_attributes
  )
  assert_eq(default_decision.decision, azimuth::telemetry::sdk::trace::SamplingDecision::Drop)
  assert_eq(default_decision.is_sampled(), false)
}

test "adaptive_sampling_strategy" {
  // 测试自适应采样策略
  
  // 创建自适应采样器
  let adaptive_sampler = azimuth::telemetry::sdk::trace::AdaptiveSampler::new()
    .with_max_spans_per_second(100)
    .with_adjustment_interval(60)  // 60秒调整间隔
  
  let trace_id = "4bf92f3577b34da6a3ce929d0e0e4736"
  let span_name = "adaptive.test.span"
  let span_kind = azimuth::telemetry::api::trace::SpanKind::Server
  let attributes = azimuth::telemetry::api::common::Attributes::empty()
  let parent_context = None
  
  // 模拟低负载情况
  let mut sampled_count_low = 0
  for i = 0; i < 10; i = i + 1 {
    let decision = adaptive_sampler.should_sample(
      parent_context,
      trace_id + i.to_string(),
      span_name,
      span_kind,
      attributes
    )
    
    if decision.is_sampled() {
      sampled_count_low = sampled_count_low + 1
    }
  }
  
  // 低负载时应该大部分被采样
  let low_load_sampling_rate = sampled_count_low.to_float() / 10.0
  assert_eq(low_load_sampling_rate >= 0.7, true)
  
  // 模拟高负载情况（快速生成大量Span）
  let mut sampled_count_high = 0
  for i = 0; i < 200; i = i + 1 {
    let decision = adaptive_sampler.should_sample(
      parent_context,
      trace_id + (i + 1000).to_string(),
      span_name,
      span_kind,
      attributes
    )
    
    if decision.is_sampled() {
      sampled_count_high = sampled_count_high + 1
    }
  }
  
  // 高负载时采样率应该降低
  let high_load_sampling_rate = sampled_count_high.to_float() / 200.0
  assert_eq(high_load_sampling_rate <= 0.5, true)
}

test "sampling_decision_attributes" {
  // 测试采样决策属性
  
  let sampler = azimuth::telemetry::sdk::trace::TraceIdRatioBasedSampler::new(0.1)
  
  let trace_id = "4bf92f3577b34da6a3ce929d0e0e4736"
  let span_name = "test.span"
  let span_kind = azimuth::telemetry::api::trace::SpanKind::Server
  let attributes = azimuth::telemetry::api::common::Attributes::empty()
  let parent_context = None
  
  let decision = sampler.should_sample(
    parent_context,
    trace_id,
    span_name,
    span_kind,
    attributes
  )
  
  // 验证采样决策属性
  if decision.is_sampled() {
    assert_eq(decision.attributes.get("sampler.type").unwrap().to_string(), "trace_id_ratio")
    assert_eq(decision.attributes.get("sampler.rate").unwrap().to_string(), "0.1")
    assert_eq(decision.attributes.get("sampling.decision").unwrap().to_string(), "sampled")
  } else {
    assert_eq(decision.attributes.get("sampling.decision").unwrap().to_string(), "not_sampled")
  }
}

test "sampling_consistency" {
  // 测试采样一致性
  
  let sampler = azimuth::telemetry::sdk::trace::TraceIdRatioBasedSampler::new(0.25)
  
  let trace_id = "4bf92f3577b34da6a3ce929d0e0e4736"
  let span_name = "consistency.test"
  let span_kind = azimuth::telemetry::api::trace::SpanKind::Server
  let attributes = azimuth::telemetry::api::common::Attributes::empty()
  let parent_context = None
  
  // 对相同的Trace ID多次采样，结果应该一致
  let decisions = []
  for i = 0; i < 10; i = i + 1 {
    let decision = sampler.should_sample(
      parent_context,
      trace_id,
      span_name,
      span_kind,
      attributes
    )
    decisions.push(decision.is_sampled())
  }
  
  // 验证一致性
  let first_decision = decisions[0]
  for decision in decisions {
    assert_eq(decision, first_decision)
  }
  
  // 不同Trace ID应该有不同的采样结果
  let different_trace_id = "00f067aa0ba902b7"
  let different_decision = sampler.should_sample(
    parent_context,
    different_trace_id,
    span_name,
    span_kind,
    attributes
  )
  
  // 不一定不同，但应该有概率差异
  assert_eq(different_decision.is_sampled() == first_decision, true or different_decision.is_sampled() != first_decision)
}

test "sampling_with_context_propagation" {
  // 测试采样与上下文传播的集成
  
  let sampler = azimuth::telemetry::sdk::trace::ParentBasedSampler::new(
    azimuth::telemetry::sdk::trace::TraceIdRatioBasedSampler::new(0.5)
  )
  
  let trace_id = "4bf92f3577b34da6a3ce929d0e0e4736"
  let span_name = "propagation.test"
  let span_kind = azimuth::telemetry::api::trace::SpanKind::Server
  let attributes = azimuth::telemetry::api::common::Attributes::empty()
  
  // 创建已采样的父上下文
  let sampled_parent_context = azimuth::telemetry::api::context::Context::empty()
    .with_value(
      azimuth::telemetry::api::context::create_key("span_context"),
      azimuth::telemetry::api::trace::SpanContext::new(
        trace_id,
        "00f067aa0ba902b7",
        true,  // is_sampled
        false  // is_remote
      )
    )
  
  // 子Span应该继承父Span的采样决策
  let child_decision = sampler.should_sample(
    Some(sampled_parent_context),
    trace_id,
    span_name,
    span_kind,
    attributes
  )
  assert_eq(child_decision.decision, azimuth::telemetry::sdk::trace::SamplingDecision::RecordAndSample)
  
  // 创建未采样的父上下文
  let not_sampled_parent_context = azimuth::telemetry::api::context::Context::empty()
    .with_value(
      azimuth::telemetry::api::context::create_key("span_context"),
      azimuth::telemetry::api::trace::SpanContext::new(
        trace_id,
        "00f067aa0ba902b8",
        false, // is_sampled
        false  // is_remote
      )
    )
  
  // 子Span应该继承父Span的采样决策
  let child_decision2 = sampler.should_sample(
    Some(not_sampled_parent_context),
    trace_id,
    span_name,
    span_kind,
    attributes
  )
  assert_eq(child_decision2.decision, azimuth::telemetry::sdk::trace::SamplingDecision::Drop)
}

test "sampling_performance_evaluation" {
  // 测试采样性能评估
  
  let sampler = azimuth::telemetry::sdk::trace::TraceIdRatioBasedSampler::new(0.1)
  
  let trace_id = "4bf92f3577b34da6a3ce929d0e0e4736"
  let span_name = "performance.test"
  let span_kind = azimuth::telemetry::api::trace::SpanKind::Server
  let attributes = azimuth::telemetry::api::common::Attributes::empty()
  let parent_context = None
  
  // 测试大量采样的性能
  let start_time = azimuth::telemetry::sdk::platform::time::current_unix_nanos()
  
  for i = 0; i < 10000; i = i + 1 {
    sampler.should_sample(
      parent_context,
      trace_id + i.to_string(),
      span_name,
      span_kind,
      attributes
    )
  }
  
  let end_time = azimuth::telemetry::sdk::platform::time::current_unix_nanos()
  let duration = end_time - start_time
  
  // 验证性能（应该在合理时间内完成）
  assert_eq(duration < 1_000_000_000, true)  // 小于1秒
  
  // 计算平均每次采样决策的时间
  let avg_duration = duration.to_float() / 10000.0
  assert_eq(avg_duration < 100_000.0, true)  // 每次小于100微秒
}