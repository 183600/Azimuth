// 自适应采样策略测试用例
// 测试自适应采样、动态采样率调整、基于属性的采样等高级采样功能

test "adaptive_sampling_based_on_throughput" {
  // 测试基于吞吐量的自适应采样
  
  struct ThroughputMetrics {
    spans_per_second: Int
    bytes_per_second: Int
    error_rate: Float
    cpu_usage: Float
    memory_usage: Float
  }
  
  struct AdaptiveSamplingConfig {
    min_sampling_rate: Float
    max_sampling_rate: Float
    target_throughput: Int
    adjustment_factor: Float
    evaluation_window_seconds: Int
  }
  
  struct AdaptiveSampler {
    current_rate: Float
    config: AdaptiveSamplingConfig
    last_evaluation_time: Int64
    recent_metrics: Array<ThroughputMetrics>
  }
  
  // 创建自适应采样配置
  let adaptive_config = AdaptiveSamplingConfig{
    min_sampling_rate: 0.01,  // 1%
    max_sampling_rate: 1.0,   // 100%
    target_throughput: 1000,   // 目标1000 spans/second
    adjustment_factor: 0.1,    // 10%调整因子
    evaluation_window_seconds: 60  // 1分钟评估窗口
  }
  
  // 创建自适应采样器
  let adaptive_sampler = AdaptiveSampler{
    current_rate: 0.1,  // 初始10%采样率
    config: adaptive_config,
    last_evaluation_time: 1640995200000000L,
    recent_metrics: Array::empty()
  }
  
  // 计算新的采样率
  let calculate_adaptive_rate = fn(sampler: AdaptiveSampler, current_metrics: ThroughputMetrics) -> Float {
    let current_throughput = current_metrics.spans_per_second
    let target_throughput = sampler.config.target_throughput
    
    let new_rate = if current_throughput > target_throughput {
      // 吞吐量过高，降低采样率
      let reduction = (current_throughput - target_throughput).to_float() / target_throughput.to_float()
      let adjusted_rate = sampler.current_rate * (1.0 - reduction * sampler.config.adjustment_factor)
      adjusted_rate.max(sampler.config.min_sampling_rate)
    } else if current_throughput < target_throughput * 0.8 {
      // 吞吐量过低，提高采样率
      let increase = (target_throughput - current_throughput).to_float() / target_throughput.to_float()
      let adjusted_rate = sampler.current_rate * (1.0 + increase * sampler.config.adjustment_factor)
      adjusted_rate.min(sampler.config.max_sampling_rate)
    } else {
      // 吞吐量在目标范围内，保持当前采样率
      sampler.current_rate
    }
    
    // 考虑错误率和资源使用情况
    let resource_factor = if current_metrics.error_rate > 0.1 || current_metrics.cpu_usage > 0.8 {
      0.8  // 高错误率或CPU使用率，降低采样
    } else if current_metrics.error_rate < 0.01 && current_metrics.cpu_usage < 0.5 {
      1.2  // 低错误率和CPU使用率，可以提高采样
    } else {
      1.0  // 正常情况
    }
    
    let final_rate = new_rate * resource_factor
    final_rate.max(sampler.config.min_sampling_rate).min(sampler.config.max_sampling_rate)
  }
  
  // 测试不同吞吐量场景
  let test_scenarios = [
    ThroughputMetrics{
      spans_per_second: 500,   // 低于目标
      bytes_per_second: 1024 * 500,
      error_rate: 0.02,
      cpu_usage: 0.3,
      memory_usage: 0.4
    },
    ThroughputMetrics{
      spans_per_second: 1000,  // 达到目标
      bytes_per_second: 1024 * 1000,
      error_rate: 0.05,
      cpu_usage: 0.5,
      memory_usage: 0.6
    },
    ThroughputMetrics{
      spans_per_second: 2000,  // 超过目标
      bytes_per_second: 1024 * 2000,
      error_rate: 0.08,
      cpu_usage: 0.7,
      memory_usage: 0.8
    },
    ThroughputMetrics{
      spans_per_second: 3000,  // 大幅超过目标
      bytes_per_second: 1024 * 3000,
      error_rate: 0.15,  // 高错误率
      cpu_usage: 0.9,   // 高CPU使用率
      memory_usage: 0.9
    }
  ]
  
  let mut current_rate = adaptive_sampler.current_rate
  
  let mut i = 0
  while i < test_scenarios.length() {
    let scenario = test_scenarios[i]
    let new_rate = calculate_adaptive_rate(adaptive_sampler, scenario)
    
    // 验证采样率在合理范围内
    assert_eq(new_rate >= adaptive_config.min_sampling_rate, true)
    assert_eq(new_rate <= adaptive_config.max_sampling_rate, true)
    
    // 验证采样率调整趋势
    match i {
      0 => {
        // 低吞吐量场景，采样率应该增加
        assert_eq(new_rate > current_rate, true)
      }
      1 => {
        // 目标吞吐量场景，采样率应该相对稳定
        let rate_change = abs(new_rate - current_rate)
        assert_eq(rate_change < 0.05, true)  // 变化小于5%
      }
      2 => {
        // 高吞吐量场景，采样率应该减少
        assert_eq(new_rate < current_rate, true)
      }
      3 => {
        // 极高吞吐量+高错误率，采样率应该大幅减少
        assert_eq(new_rate < current_rate, true)
        assert_eq(new_rate <= adaptive_config.min_sampling_rate * 2.0, true)
      }
      _ => {}
    }
    
    current_rate = new_rate
    i = i + 1
  }
}

test "attribute_based_sampling_decisions" {
  // 测试基于属性的采样决策
  
  struct AttributeRule {
    attribute_name: String
    attribute_pattern: String
    sampling_rate: Float
    priority: Int
  }
  
  struct AttributeBasedSampler {
    default_rate: Float
    rules: Array<AttributeRule>
    fallback_enabled: Bool
  }
  
  // 创建基于属性的采样器
  let attribute_sampler = AttributeBasedSampler{
    default_rate: 0.1,  // 默认10%采样
    rules: [
      AttributeRule{
        attribute_name: "http.method",
        attribute_pattern: "GET|POST",
        sampling_rate: 0.2,  // GET和POST请求20%采样
        priority: 1
      },
      AttributeRule{
        attribute_name: "http.status_code",
        attribute_pattern: "4[0-9][0-9]|5[0-9][0-9]",
        sampling_rate: 1.0,  // 错误响应100%采样
        priority: 2
      },
      AttributeRule{
        attribute_name: "service.name",
        attribute_pattern: "critical-service|payment-service",
        sampling_rate: 0.5,  // 关键服务50%采样
        priority: 3
      },
      AttributeRule{
        attribute_name: "user.id",
        attribute_pattern: "premium-.*",
        sampling_rate: 0.8,  // 付费用户80%采样
        priority: 4
      }
    ],
    fallback_enabled: true
  }
  
  // 模拟正则表达式匹配（简化）
  let pattern_matches = fn(value: String, pattern: String) -> Bool {
    if pattern.contains("|") {
      // 处理或模式
      let alternatives = pattern"|".split_to_string()
      let mut i = 0
      while i < alternatives.length() {
        let alt = alternatives[i]
        if alt.contains("[") && alt.contains("]") {
          // 处理字符类（简化）
          if alt == "4[0-9][0-9]" && value.length() == 3 && value[0] == '4' {
            return true
          } else if alt == "5[0-9][0-9]" && value.length() == 3 && value[0] == '5' {
            return true
          }
        } else if alt.contains(".*") {
          // 处理通配符（简化）
          let prefix = alt.substring(0, alt.length() - 2)
          if value.has_prefix(prefix) {
            return true
          }
        } else if value == alt {
          return true
        }
        i = i + 1
      }
      false
    } else if pattern.contains(".*") {
      let prefix = pattern.substring(0, pattern.length() - 2)
      value.has_prefix(prefix)
    } else {
      value == pattern
    }
  }
  
  // 计算基于属性的采样率
  let calculate_attribute_based_rate = fn(
    sampler: AttributeBasedSampler, 
    attributes: Array<(String, String)>
  ) -> Float {
    let mut matching_rules = Array::empty<AttributeRule>()
    
    // 查找匹配的规则
    let mut i = 0
    while i < sampler.rules.length() {
      let rule = sampler.rules[i]
      
      let mut j = 0
      while j < attributes.length() {
        let (attr_name, attr_value) = attributes[j]
        if attr_name == rule.attribute_name && pattern_matches(attr_value, rule.attribute_pattern) {
          matching_rules.push(rule)
          break
        }
        j = j + 1
      }
      
      i = i + 1
    }
    
    // 按优先级排序，选择最高优先级的规则
    if matching_rules.length() > 0 {
      matching_rules.sort_by(fn(a, b) { a.priority - b.priority })
      matching_rules[0].sampling_rate
    } else {
      sampler.default_rate
    }
  }
  
  // 测试不同属性组合
  let test_cases = [
    (
      [("http.method", "GET"), ("http.status_code", "200")],
      0.2  // 匹配GET方法规则
    ),
    (
      [("http.method", "PUT"), ("http.status_code", "404")],
      1.0  // 匹配404状态码规则（优先级更高）
    ),
    (
      [("service.name", "critical-service"), ("http.method", "POST")],
      0.5  // 匹配关键服务规则（优先级更高）
    ),
    (
      [("user.id", "premium-12345"), ("http.method", "GET")],
      0.8  // 匹配付费用户规则（优先级最高）
    ),
    (
      [("http.method", "DELETE"), ("http.status_code", "200")],
      0.1  // 不匹配任何规则，使用默认采样率
    ),
    (
      [("service.name", "regular-service"), ("user.id", "regular-123")],
      0.1  // 不匹配任何规则，使用默认采样率
    )
  ]
  
  let mut i = 0
  while i < test_cases.length() {
    let (attributes, expected_rate) = test_cases[i]
    let calculated_rate = calculate_attribute_based_rate(attribute_sampler, attributes)
    
    assert_eq(calculated_rate, expected_rate, 
      "Test case {i}: expected rate {expected_rate}, got {calculated_rate}")
    
    i = i + 1
  }
  
  // 测试规则优先级
  let high_priority_attributes = [
    ("http.status_code", "500"),
    ("service.name", "critical-service"),
    ("user.id", "premium-123"),
    ("http.method", "GET")
  ]
  
  let high_priority_rate = calculate_attribute_based_rate(attribute_sampler, high_priority_attributes)
  assert_eq(high_priority_rate, 1.0)  // 应该匹配错误状态码规则（优先级2）
  
  // 测试无匹配情况下的回退
  let no_match_attributes = [
    ("custom.attribute", "custom.value"),
    ("another.attribute", "another.value")
  ]
  
  let no_match_rate = calculate_attribute_based_rate(attribute_sampler, no_match_attributes)
  assert_eq(no_match_rate, attribute_sampler.default_rate)
}

test "dynamic_sampling_rate_adjustment" {
  // 测试动态采样率调整
  
  enum AdjustmentTrigger {
    TimeBased(Int)      // 基于时间的调整（秒）
    VolumeBased(Int)    // 基于数据量的调整（span数量）
    ErrorBased(Float)   // 基于错误率的调整（错误率阈值）
    CostBased(Int)      // 基于成本的调整（成本单位）
  }
  
  struct DynamicAdjustmentConfig {
    triggers: Array<AdjustmentTrigger>
    min_rate: Float
    max_rate: Float
    adjustment_step: Float
    cooldown_period_seconds: Int
  }
  
  struct SamplingHistory {
    timestamp: Int64
    rate: Float
    spans_sampled: Int
    spans_total: Int
    adjustment_reason: String
  }
  
  struct DynamicSampler {
    current_rate: Float
    config: DynamicAdjustmentConfig
    history: Array<SamplingHistory>
    last_adjustment_time: Int64
    accumulated_spans: Int
    current_error_rate: Float
  }
  
  // 创建动态调整配置
  let dynamic_config = DynamicAdjustmentConfig{
    triggers: [
      TimeBased(300),        // 每5分钟
      VolumeBased(10000),    // 每10000个span
      ErrorBased(0.1),       // 错误率超过10%
      CostBased(1000)        // 成本超过1000单位
    ],
    min_rate: 0.01,
    max_rate: 1.0,
    adjustment_step: 0.05,
    cooldown_period_seconds: 60
  }
  
  // 创建动态采样器
  let dynamic_sampler = DynamicSampler{
    current_rate: 0.1,
    config: dynamic_config,
    history: Array::empty(),
    last_adjustment_time: 1640995200000000L,
    accumulated_spans: 0,
    current_error_rate: 0.05
  }
  
  // 检查是否需要调整
  let should_adjust = fn(sampler: DynamicSampler, current_time: Int64) -> (Bool, String) {
    let mut reasons = Array::empty<String>()
    
    // 检查时间触发器
    let mut i = 0
    while i < sampler.config.triggers.length() {
      let trigger = sampler.config.triggers[i]
      match trigger {
        TimeBased(seconds) => {
          let time_diff = (current_time - sampler.last_adjustment_time) / 1000000000L
          if time_diff >= seconds.to_int64() {
            reasons.push("time_based")
          }
        }
        VolumeBased(span_count) => {
          if sampler.accumulated_spans >= span_count {
            reasons.push("volume_based")
          }
        }
        ErrorBased(threshold) => {
          if sampler.current_error_rate >= threshold {
            reasons.push("error_based")
          }
        }
        CostBased(cost_units) => {
          // 简化成本计算：假设每个采样的span成本1单位
          let estimated_cost = sampler.history.fold(0, fn(acc, h) { acc + h.spans_sampled })
          if estimated_cost >= cost_units {
            reasons.push("cost_based")
          }
        }
      }
      i = i + 1
    }
    
    if reasons.length() > 0 {
      (true, reasons.join(","))
    } else {
      (false, "")
    }
  }
  
  // 执行动态调整
  let perform_adjustment = fn(sampler: DynamicSampler, reason: String) -> DynamicSampler {
    let new_rate = if reason.contains("error_based") {
      // 错误率触发，提高采样率以获取更多错误信息
      (sampler.current_rate + sampler.config.adjustment_step).min(sampler.config.max_rate)
    } else if reason.contains("cost_based") {
      // 成本触发，降低采样率以控制成本
      (sampler.current_rate - sampler.config.adjustment_step).max(sampler.config.min_rate)
    } else if reason.contains("volume_based") {
      // 数据量触发，根据当前负载调整
      if sampler.accumulated_spans > 50000 {
        // 高负载，降低采样率
        (sampler.current_rate - sampler.config.adjustment_step).max(sampler.config.min_rate)
      } else {
        // 中等负载，保持或略微提高
        (sampler.current_rate + sampler.config.adjustment_step * 0.5).min(sampler.config.max_rate)
      }
    } else {
      // 时间触发，根据历史趋势调整
      let recent_avg_rate = if sampler.history.length() >= 2 {
        let recent = sampler.history.slice(sampler.history.length() - 2, sampler.history.length())
        (recent[0].rate + recent[1].rate) / 2.0
      } else {
        sampler.current_rate
      }
      
      if recent_avg_rate > sampler.current_rate {
        // 历史呈上升趋势，继续提高
        (sampler.current_rate + sampler.config.adjustment_step * 0.5).min(sampler.config.max_rate)
      } else {
        // 历史稳定或下降，保持当前率
        sampler.current_rate
      }
    }
    
    // 重置累积计数器
    { sampler |
      current_rate: new_rate,
      accumulated_spans: 0
    }
  }
  
  // 测试不同触发场景
  let current_time = 1640995200000000L
  
  // 场景1：时间触发调整
  let time_triggered_sampler = { dynamic_sampler | last_adjustment_time: current_time - 400000000000L }  // 400秒前
  let (should_adjust_time, reason_time) = should_adjust(time_triggered_sampler, current_time)
  assert_eq(should_adjust_time, true)
  assert_eq(reason_time.contains("time_based"), true)
  
  let adjusted_time_sampler = perform_adjustment(time_triggered_sampler, reason_time)
  assert_eq(adjusted_time_sampler.current_rate > dynamic_sampler.current_rate, true)
  assert_eq(adjusted_time_sampler.accumulated_spans, 0)
  
  // 场景2：错误率触发调整
  let error_triggered_sampler = { dynamic_sampler | current_error_rate: 0.15 }  // 15%错误率
  let (should_adjust_error, reason_error) = should_adjust(error_triggered_sampler, current_time)
  assert_eq(should_adjust_error, true)
  assert_eq(reason_error.contains("error_based"), true)
  
  let adjusted_error_sampler = perform_adjustment(error_triggered_sampler, reason_error)
  assert_eq(adjusted_error_sampler.current_rate > dynamic_sampler.current_rate, true)
  
  // 场景3：成本触发调整
  let cost_triggered_sampler = { dynamic_sampler |
    accumulated_spans = 0,
    history = [
      SamplingHistory{
        timestamp: current_time - 1000000000L,
        rate: 0.1,
        spans_sampled: 500,
        spans_total: 5000,
        adjustment_reason: "initial"
      },
      SamplingHistory{
        timestamp: current_time - 500000000L,
        rate: 0.1,
        spans_sampled: 600,
        spans_total: 6000,
        adjustment_reason: "time_based"
      }
    ]
  }
  
  let (should_adjust_cost, reason_cost) = should_adjust(cost_triggered_sampler, current_time)
  assert_eq(should_adjust_cost, true)
  assert_eq(reason_cost.contains("cost_based"), true)
  
  let adjusted_cost_sampler = perform_adjustment(cost_triggered_sampler, reason_cost)
  assert_eq(adjusted_cost_sampler.current_rate < dynamic_sampler.current_rate, true)
  
  // 场景4：数据量触发调整
  let volume_triggered_sampler = { dynamic_sampler | accumulated_spans = 15000 }  // 15000个span
  let (should_adjust_volume, reason_volume) = should_adjust(volume_triggered_sampler, current_time)
  assert_eq(should_adjust_volume, true)
  assert_eq(reason_volume.contains("volume_based"), true)
  
  let adjusted_volume_sampler = perform_adjustment(volume_triggered_sampler, reason_volume)
  assert_eq(adjusted_volume_sampler.current_rate < dynamic_sampler.current_rate, true)  // 高负载，降低采样率
  assert_eq(adjusted_volume_sampler.accumulated_spans, 0)
  
  // 验证采样率边界
  assert_eq(adjusted_time_sampler.current_rate <= dynamic_config.max_rate, true)
  assert_eq(adjusted_time_sampler.current_rate >= dynamic_config.min_rate, true)
  assert_eq(adjusted_cost_sampler.current_rate >= dynamic_config.min_rate, true)
}

test "sampling_consistency_in_distributed_systems" {
  // 测试分布式系统中的采样一致性
  
  struct SamplingContext {
    trace_id: String
    parent_sampled: Option<Bool>
    service_name: String
    sampling_decision: Bool
    sampling_attributes: Array<(String, String)>
  }
  
  struct DistributedSamplingConfig {
    ensure_trace_completeness: Bool
    parent_priority: Bool
    fallback_strategy: String  // "respect_parent", "local_decision", "adaptive"
  }
  
  struct DistributedSampler {
    config: DistributedSamplingConfig
    local_sampling_rate: Float
    service_name: String
  }
  
  // 创建分布式采样器
  let distributed_config = DistributedSamplingConfig{
    ensure_trace_completeness: true,
    parent_priority: true,
    fallback_strategy: "respect_parent"
  }
  
  let distributed_sampler = DistributedSampler{
    config: distributed_config,
    local_sampling_rate: 0.1,
    service_name: "test-service"
  }
  
  // 计算分布式采样决策
  let calculate_distributed_sampling = fn(
    sampler: DistributedSampler,
    trace_id: String,
    parent_sampled: Option[Bool],
    local_attributes: Array<(String, String)>
  ) -> SamplingContext {
    let sampling_decision = match parent_sampled {
      Some(parent_decision) => {
        if sampler.config.parent_priority {
          // 尊重父级决策
          parent_decision
        } else {
          // 使用本地决策（简化）
          trace_id.to_int() % 10 == 0  // 10%采样
        }
      }
      None => {
        // 没有父级，使用本地决策
        trace_id.to_int() % 10 == 0  // 10%采样
      }
    }
    
    let sampling_attributes = [
      ("sampler.type", "distributed"),
      ("sampler.service", sampler.service_name),
      ("sampler.decision", if sampling_decision { "record_and_sample" } else { "drop" })
    ].concat(
      match parent_sampled {
        Some(parent_decision) => [
          ("sampler.parent_decision", parent_decision.to_string()),
          ("sampler.parent_priority", sampler.config.parent_priority.to_string())
        ]
        None => [
          ("sampler.parent_decision", "none"),
          ("sampler.is_root", "true")
        ]
      }
    )
    
    SamplingContext{
      trace_id: trace_id,
      parent_sampled: parent_sampled,
      service_name: sampler.service_name,
      sampling_decision: sampling_decision,
      sampling_attributes: sampling_attributes
    }
  }
  
  // 测试跨服务的采样一致性
  let test_trace_id = "0123456789abcdef0123456789abcdef"
  let services = ["gateway", "auth-service", "user-service", "order-service", "payment-service"]
  
  // 场景1：根span被采样
  let root_context = calculate_distributed_sampling(
    distributed_sampler,
    test_trace_id,
    None,  // 没有父级
    Array::empty()
  )
  
  let mut child_contexts = Array::empty<SamplingContext>()
  let mut i = 0
  while i < services.length() {
    let service_sampler = { distributed_sampler | service_name = services[i] }
    let child_context = calculate_distributed_sampling(
      service_sampler,
      test_trace_id,
      Some(root_context.sampling_decision),  // 父级决策
      Array::empty()
    )
    child_contexts.push(child_context)
    i = i + 1
  }
  
  // 验证采样一致性
  let mut j = 0
  while j < child_contexts.length() {
    let child_ctx = child_contexts[j]
    assert_eq(child_ctx.sampling_decision, root_context.sampling_decision,
      "Child service {child_ctx.service_name} should respect parent sampling decision")
    assert_eq(child_ctx.parent_sampled, Some(root_context.sampling_decision))
    j = j + 1
  }
  
  // 场景2：根span未被采样
  let root_context_unsampled = { root_context | sampling_decision = false }
  
  let mut child_contexts_unsampled = Array::empty<SamplingContext>()
  let mut k = 0
  while k < services.length() {
    let service_sampler = { distributed_sampler | service_name = services[k] }
    let child_context = calculate_distributed_sampling(
      service_sampler,
      test_trace_id,
      Some(root_context_unsampled.sampling_decision),
      Array::empty()
    )
    child_contexts_unsampled.push(child_context)
    k = k + 1
  }
  
  // 验证未采样的根span下所有子span也不被采样
  let mut l = 0
  while l < child_contexts_unsampled.length() {
    let child_ctx = child_contexts_unsampled[l]
    assert_eq(child_ctx.sampling_decision, false,
      "Child service {child_ctx.service_name} should not be sampled when parent is not sampled")
    l = l + 1
  }
  
  // 场景3：测试采样属性的一致性
  let sample_attributes = root_context.sampling_attributes
  assert_eq(sample_attributes.contains(("sampler.type", "distributed")), true)
  assert_eq(sample_attributes.contains(("sampler.service", "test-service")), true)
  assert_eq(sample_attributes.contains(("sampler.is_root", "true")), true)
  
  let child_sample_attributes = child_contexts[0].sampling_attributes
  assert_eq(child_sample_attributes.contains(("sampler.type", "distributed")), true)
  assert_eq(child_sample_attributes.contains(("sampler.service", "gateway")), true)
  assert_eq(child_sample_attributes.contains(("sampler.parent_decision", "true")), true)
  assert_eq(child_sample_attributes.contains(("sampler.parent_priority", "true")), true)
  
  // 场景4：测试不同fallback策略
  let adaptive_config = { distributed_config | fallback_strategy = "adaptive" }
  let adaptive_sampler = { distributed_sampler | config = adaptive_config }
  
  let adaptive_context = calculate_distributed_sampling(
    adaptive_sampler,
    test_trace_id,
    Some(false),  // 父级未采样
    Array::empty()
  )
  
  // 在自适应模式下，即使父级未采样，本地也可能采样（简化实现）
  // 这里我们保持一致性，但实际实现可能更复杂
  assert_eq(adaptive_context.sampling_decision, false)
}

test "sampling_performance_under_load" {
  // 测试高负载下的采样性能
  
  struct PerformanceMetrics {
    sampler_type: String
    operations_per_second: Int
    avg_latency_ns: Int
    p99_latency_ns: Int
    memory_usage_mb: Float
    cpu_usage_percent: Float
  }
  
  // 模拟不同采样器的性能特征
  let get_sampler_performance = fn(sampler_type: String, load: Int) -> PerformanceMetrics {
    match sampler_type {
      "always_on" => PerformanceMetrics{
        sampler_type: sampler_type,
        operations_per_second: load,
        avg_latency_ns: 50,    // 50ns
        p99_latency_ns: 100,   // 100ns
        memory_usage_mb: 0.1,
        cpu_usage_percent: 0.5
      }
      "always_off" => PerformanceMetrics{
        sampler_type: sampler_type,
        operations_per_second: load,
        avg_latency_ns: 45,    // 45ns
        p99_latency_ns: 90,    // 90ns
        memory_usage_mb: 0.1,
        cpu_usage_percent: 0.4
      }
      "trace_id_ratio" => PerformanceMetrics{
        sampler_type: sampler_type,
        operations_per_second: load,
        avg_latency_ns: 200,   // 200ns（需要计算hash）
        p99_latency_ns: 400,   // 400ns
        memory_usage_mb: 0.2,
        cpu_usage_percent: 2.0
      }
      "attribute_based" => PerformanceMetrics{
        sampler_type: sampler_type,
        operations_per_second: load,
        avg_latency_ns: 500,   // 500ns（需要匹配属性）
        p99_latency_ns: 1000,  // 1000ns
        memory_usage_mb: 0.5,
        cpu_usage_percent: 5.0
      }
      "adaptive" => PerformanceMetrics{
        sampler_type: sampler_type,
        operations_per_second: load,
        avg_latency_ns: 800,   // 800ns（需要评估指标）
        p99_latency_ns: 1500,  // 1500ns
        memory_usage_mb: 1.0,
        cpu_usage_percent: 8.0
      }
      _ => PerformanceMetrics{
        sampler_type: "unknown",
        operations_per_second: load,
        avg_latency_ns: 1000,
        p99_latency_ns: 2000,
        memory_usage_mb: 2.0,
        cpu_usage_percent: 10.0
      }
    }
  }
  
  // 测试不同负载下的性能
  let load_levels = [1000, 10000, 100000, 1000000]  // 每秒操作数
  let sampler_types = ["always_on", "always_off", "trace_id_ratio", "attribute_based", "adaptive"]
  
  let mut i = 0
  while i < load_levels.length() {
    let load = load_levels[i]
    
    let mut j = 0
    while j < sampler_types.length() {
      let sampler_type = sampler_types[j]
      let metrics = get_sampler_performance(sampler_type, load)
      
      // 验证性能指标
      assert_eq(metrics.sampler_type, sampler_type)
      assert_eq(metrics.operations_per_second, load)
      assert_eq(metrics.avg_latency_ns > 0, true)
      assert_eq(metrics.p99_latency_ns >= metrics.avg_latency_ns, true)
      assert_eq(metrics.memory_usage_mb > 0.0, true)
      assert_eq(metrics.cpu_usage_percent > 0.0, true)
      
      // 性能基准验证
      match sampler_type {
        "always_on" | "always_off" => {
          // 简单采样器应该很快
          assert_eq(metrics.avg_latency_ns <= 100, true,
            "{sampler_type} should have avg latency <= 100ns under load {load}")
          assert_eq(metrics.cpu_usage_percent <= 1.0, true,
            "{sampler_type} should use <= 1% CPU under load {load}")
        }
        "trace_id_ratio" => {
          assert_eq(metrics.avg_latency_ns <= 500, true,
            "{sampler_type} should have avg latency <= 500ns under load {load}")
          assert_eq(metrics.cpu_usage_percent <= 5.0, true,
            "{sampler_type} should use <= 5% CPU under load {load}")
        }
        "attribute_based" => {
          assert_eq(metrics.avg_latency_ns <= 1000, true,
            "{sampler_type} should have avg latency <= 1000ns under load {load}")
          assert_eq(metrics.cpu_usage_percent <= 10.0, true,
            "{sampler_type} should use <= 10% CPU under load {load}")
        }
        "adaptive" => {
          assert_eq(metrics.avg_latency_ns <= 2000, true,
            "{sampler_type} should have avg latency <= 2000ns under load {load}")
          assert_eq(metrics.cpu_usage_percent <= 15.0, true,
            "{sampler_type} should use <= 15% CPU under load {load}")
        }
        _ => {}
      }
      
      // 内存使用验证
      assert_eq(metrics.memory_usage_mb <= 5.0, true,
        "{sampler_type} should use <= 5MB memory under load {load}")
      
      j = j + 1
    }
    
    i = i + 1
  }
  
  // 测试性能缩放
  let baseline_metrics = get_sampler_performance("trace_id_ratio", 1000)
  let high_load_metrics = get_sampler_performance("trace_id_ratio", 1000000)
  
  // 在高负载下，延迟不应该线性增长（应该有优化）
  let latency_ratio = high_load_metrics.avg_latency_ns.to_float() / baseline_metrics.avg_latency_ns.to_float()
  let load_ratio = 1000000.to_float() / 1000.to_float()
  
  // 延迟增长应该远小于负载增长（理想情况下）
  assert_eq(latency_ratio < load_ratio * 0.1, true,
    "Latency should scale much better than linear load increase")
  
  // CPU使用应该合理
  assert_eq(high_load_metrics.cpu_usage_percent <= 50.0, true,
    "CPU usage should remain reasonable under high load")
}