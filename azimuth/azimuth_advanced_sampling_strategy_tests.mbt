// 高级采样策略测试用例
// 测试基于概率和属性的自适应采样机制

test "概率采样器边界条件测试" {
  // 创建不同概率的采样器
  let zero_sampler = azimuth::ProbabilitySampler::new(0.0)
  let full_sampler = azimuth::ProbabilitySampler::new(1.0)
  let half_sampler = azimuth::ProbabilitySampler::new(0.5)
  let tiny_sampler = azimuth::ProbabilitySampler::new(0.001)  // 0.1%
  
  let trace_id = "probability-test-trace"
  let span_name = "probability-test-span"
  let span_kind = azimuth::Internal
  let parent_context = azimuth::SpanContext::new(trace_id, "parent-span", true, "")
  let empty_attributes = []
  
  // 测试0%采样率 - 应该总是不采样
  let zero_decision = azimuth::Sampler::should_sample(
    zero_sampler, parent_context, trace_id, span_name, span_kind, empty_attributes
  )
  assert_false(azimuth::SamplingDecision::is_sampled(zero_decision))
  
  // 测试100%采样率 - 应该总是采样
  let full_decision = azimuth::Sampler::should_sample(
    full_sampler, parent_context, trace_id, span_name, span_kind, empty_attributes
  )
  assert_true(azimuth::SamplingDecision::is_sampled(full_decision))
  
  // 测试50%采样率 - 多次测试验证概率分布
  let mut sample_count = 0
  for i = 0; i < 1000; i++ {
    let decision = azimuth::Sampler::should_sample(
      half_sampler, parent_context, trace_id, span_name, span_kind, empty_attributes
    )
    if azimuth::SamplingDecision::is_sampled(decision) {
      sample_count = sample_count + 1
    }
  }
  // 验证采样率在合理范围内 (45%-55%)
  assert_true(sample_count >= 450 and sample_count <= 550)
  
  // 测试极小概率采样率
  let mut tiny_sample_count = 0
  for i = 0; i < 10000; i++ {
    let decision = azimuth::Sampler::should_sample(
      tiny_sampler, parent_context, trace_id, span_name, span_kind, empty_attributes
    )
    if azimuth::SamplingDecision::is_sampled(decision) {
      tiny_sample_count = tiny_sample_count + 1
    }
  }
  // 验证极小概率采样 (0-20次，期望10次)
  assert_true(tiny_sample_count >= 0 and tiny_sample_count <= 20)
}

test "基于属性的智能采样器测试" {
  // 创建基于属性的智能采样器
  let attribute_sampler = azimuth::AttributeBasedSampler::new([
    // 高优先级属性 - 总是采样
    ("error", azimuth::StringValue("true")),
    ("critical.path", azimuth::StringValue("true")),
    ("security.event", azimuth::StringValue("true")),
    // 条件采样属性
    ("response.time", azimuth::IntValue(1000)),  // 响应时间>1000ms时提高采样率
    ("user.premium", azimuth::StringValue("true"))  // 付费用户提高采样率
  ])
  
  let trace_id = "attribute-test-trace"
  let span_name = "attribute-test-span"
  let span_kind = azimuth::Server
  let parent_context = azimuth::SpanContext::new(trace_id, "parent-span", true, "")
  
  // 测试错误事件 - 应该总是采样
  let error_attributes = [
    ("error", azimuth::StringValue("true")),
    ("error.message", azimuth::StringValue("Database connection failed")),
    ("error.code", azimuth::IntValue(500))
  ]
  
  let error_decision = azimuth::Sampler::should_sample(
    attribute_sampler, parent_context, trace_id, span_name, span_kind, error_attributes
  )
  assert_true(azimuth::SamplingDecision::is_sampled(error_decision))
  
  // 测试关键路径 - 应该总是采样
  let critical_attributes = [
    ("critical.path", azimuth::StringValue("true")),
    ("service.name", azimuth::StringValue("payment-service")),
    ("transaction.id", azimuth::StringValue("txn-12345"))
  ]
  
  let critical_decision = azimuth::Sampler::should_sample(
    attribute_sampler, parent_context, trace_id, span_name, span_kind, critical_attributes
  )
  assert_true(azimuth::SamplingDecision::is_sampled(critical_decision))
  
  // 测试慢响应 - 应该提高采样率
  let slow_attributes = [
    ("response.time", azimuth::IntValue(1500)),  // 1.5秒响应时间
    ("db.query.time", azimuth::IntValue(1200)),
    ("cache.miss", azimuth::BoolValue(true))
  ]
  
  let slow_decision = azimuth::Sampler::should_sample(
    attribute_sampler, parent_context, trace_id, span_name, span_kind, slow_attributes
  )
  assert_true(azimuth::SamplingDecision::is_sampled(slow_decision))
  
  // 测试付费用户 - 应该提高采样率
  let premium_attributes = [
    ("user.premium", azimuth::StringValue("true")),
    ("user.id", azimuth::StringValue("user-67890")),
    ("subscription.level", azimuth::StringValue("premium"))
  ]
  
  let premium_decision = azimuth::Sampler::should_sample(
    attribute_sampler, parent_context, trace_id, span_name, span_kind, premium_attributes
  )
  assert_true(azimuth::SamplingDecision::is_sampled(premium_decision))
  
  // 测试普通请求 - 可能不采样
  let normal_attributes = [
    ("user.id", azimuth::StringValue("user-11111")),
    ("request.method", azimuth::StringValue("GET")),
    ("response.status", azimuth::IntValue(200))
  ]
  
  let mut normal_sample_count = 0
  for i = 0; i < 100; i++ {
    let decision = azimuth::Sampler::should_sample(
      attribute_sampler, parent_context, trace_id, span_name, span_kind, normal_attributes
    )
    if azimuth::SamplingDecision::is_sampled(decision) {
      normal_sample_count = normal_sample_count + 1
    }
  }
  // 普通请求采样率应该较低 (<30%)
  assert_true(normal_sample_count < 30)
}

test "复合采样器策略测试" {
  // 创建复合采样器，组合多种采样策略
  let probability_sampler = azimuth::ProbabilitySampler::new(0.1)  // 10%基础采样率
  let attribute_sampler = azimuth::AttributeBasedSampler::new([
    ("error", azimuth::StringValue("true")),
    ("slow.query", azimuth::StringValue("true"))
  ])
  
  let composite_sampler = azimuth::CompositeSampler::new([
    ("attribute", attribute_sampler, 1.0),    // 属性采样器权重1.0
    ("probability", probability_sampler, 0.5) // 概率采样器权重0.5
  ])
  
  let trace_id = "composite-test-trace"
  let span_name = "composite-test-span"
  let span_kind = azimuth::Internal
  let parent_context = azimuth::SpanContext::new(trace_id, "parent-span", true, "")
  
  // 测试错误请求 - 属性采样器应该主导决策
  let error_attributes = [
    ("error", azimuth::StringValue("true")),
    ("error.type", azimuth::StringValue("TimeoutError"))
  ]
  
  let error_decision = azimuth::Sampler::should_sample(
    composite_sampler, parent_context, trace_id, span_name, span_kind, error_attributes
  )
  assert_true(azimuth::SamplingDecision::is_sampled(error_decision))
  
  // 测试慢查询 - 属性采样器应该主导决策
  let slow_attributes = [
    ("slow.query", azimuth::StringValue("true")),
    ("query.time", azimuth::IntValue(5000))
  ]
  
  let slow_decision = azimuth::Sampler::should_sample(
    composite_sampler, parent_context, trace_id, span_name, span_kind, slow_attributes
  )
  assert_true(azimuth::SamplingDecision::is_sampled(slow_decision))
  
  // 测试普通请求 - 概率采样器应该主导决策
  let normal_attributes = [
    ("request.id", azimuth::StringValue("req-123")),
    ("user.id", azimuth::StringValue("user-456"))
  ]
  
  let mut normal_sample_count = 0
  for i = 0; i < 200; i++ {
    let decision = azimuth::Sampler::should_sample(
      composite_sampler, parent_context, trace_id, span_name, span_kind, normal_attributes
    )
    if azimuth::SamplingDecision::is_sampled(decision) {
      normal_sample_count = normal_sample_count + 1
    }
  }
  // 普通请求采样率应该接近概率采样器的10%（考虑权重调整）
  assert_true(normal_sample_count >= 10 and normal_sample_count <= 30)
}

test "采样器性能基准测试" {
  // 创建不同类型的采样器
  let probability_sampler = azimuth::ProbabilitySampler::new(0.1)
  let attribute_sampler = azimuth::AttributeBasedSampler::new([
    ("error", azimuth::StringValue("true")),
    ("slow.query", azimuth::StringValue("true"))
  ])
  
  let trace_id = "performance-test-trace"
  let span_name = "performance-test-span"
  let span_kind = azimuth::Internal
  let parent_context = azimuth::SpanContext::new(trace_id, "parent-span", true, "")
  let test_attributes = [
    ("request.id", azimuth::StringValue("req-123")),
    ("user.id", azimuth::StringValue("user-456")),
    ("service.name", azimuth::StringValue("test-service"))
  ]
  
  // 测试概率采样器性能
  let start_time = azimuth::Clock::now()
  for i = 0; i < 10000; i++ {
    let _ = azimuth::Sampler::should_sample(
      probability_sampler, parent_context, trace_id, span_name, span_kind, test_attributes
    )
  }
  let probability_duration = azimuth::Clock::now() - start_time
  
  // 测试属性采样器性能
  let start_time = azimuth::Clock::now()
  for i = 0; i < 10000; i++ {
    let _ = azimuth::Sampler::should_sample(
      attribute_sampler, parent_context, trace_id, span_name, span_kind, test_attributes
    )
  }
  let attribute_duration = azimuth::Clock::now() - start_time
  
  // 性能断言 - 采样决策应该在合理时间内完成
  // 这里假设每次采样决策应该在1微秒内完成
  assert_true(probability_duration < 10000)  // 10ms for 10k operations
  assert_true(attribute_duration < 50000)    // 50ms for 10k operations (属性检查更复杂)
  
  // 属性采样器可能比概率采样器慢，但不应该慢太多
  assert_true(attribute_duration < probability_duration * 5)
}

test "采样器状态一致性测试" {
  // 创建自适应采样器
  let adaptive_sampler = azimuth::AdaptiveSampler::new(0.1)
  
  let trace_id = "consistency-test-trace"
  let span_name = "consistency-test-span"
  let span_kind = azimuth::Internal
  let parent_context = azimuth::SpanContext::new(trace_id, "parent-span", true, "")
  
  // 相同的输入应该产生相同的采样决策
  let test_attributes = [
    ("request.id", azimuth::StringValue("req-consistency")),
    ("user.id", azimuth::StringValue("user-consistency"))
  ]
  
  let decisions = []
  for i = 0; i < 10; i++ {
    let decision = azimuth::Sampler::should_sample(
      adaptive_sampler, parent_context, trace_id, span_name, span_kind, test_attributes
    )
    decisions.push(decision)
  }
  
  // 验证所有决策都相同
  let first_decision = decisions[0]
  for i = 1; i < 10; i++ {
    assert_eq(decisions[i], first_decision)
  }
  
  // 测试不同trace_id应该可能产生不同决策
  let different_trace_id = "different-consistency-trace"
  let different_decision = azimuth::Sampler::should_sample(
    adaptive_sampler, parent_context, different_trace_id, span_name, span_kind, test_attributes
  )
  
  // 不同trace_id的决策可能相同也可能不同，这里不做强制断言
  // 但可以验证决策的有效性
  assert_true(azimuth::SamplingDecision::is_valid(first_decision))
  assert_true(azimuth::SamplingDecision::is_valid(different_decision))
}