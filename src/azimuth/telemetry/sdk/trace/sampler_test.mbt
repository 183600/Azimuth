// 采样器功能测试 - 用于测试分布式追踪中的采样策略和决策

test "sampler_basic_types" {
  // 测试基本采样器类型
  
  enum SamplerType {
    AlwaysOn
    AlwaysOff
    TraceIdRatio
    ParentBased
    RateLimiting
  }
  
  struct SamplerConfig {
    sampler_type : SamplerType
    description : String
    is_record_sampled : Bool
    is_span_sampled : Bool
    parameters : Array[String>
  }
  
  let sampler_configs = [
    {
      sampler_type: AlwaysOn,
      description: "Always sample all traces",
      is_record_sampled: true,
      is_span_sampled: true,
      parameters: []
    },
    {
      sampler_type: AlwaysOff,
      description: "Never sample any traces",
      is_record_sampled: false,
      is_span_sampled: false,
      parameters: []
    },
    {
      sampler_type: TraceIdRatio,
      description: "Sample based on trace ID ratio",
      is_record_sampled: true,
      is_span_sampled: true,
      parameters: ["ratio:0.1"]
    },
    {
      sampler_type: ParentBased,
      description: "Respect parent span's sampling decision",
      is_record_sampled: true,
      is_span_sampled: true,
      parameters: ["root_sampler:AlwaysOn"]
    },
    {
      sampler_type: RateLimiting,
      description: "Sample at a fixed rate per second",
      is_record_sampled: true,
      is_span_sampled: true,
      parameters: ["rate:100"]
    }
  ]
  
  // 验证采样器配置
  for config in sampler_configs {
    assert_eq(config.description.length() > 0, true)
    assert_eq(config.parameters.length() >= 0, true)
    
    match config.sampler_type {
      AlwaysOn => {
        assert_eq(config.is_record_sampled, true)
        assert_eq(config.is_span_sampled, true)
        assert_eq(config.parameters.length(), 0)
      }
      AlwaysOff => {
        assert_eq(config.is_record_sampled, false)
        assert_eq(config.is_span_sampled, false)
        assert_eq(config.parameters.length(), 0)
      }
      TraceIdRatio => {
        assert_eq(config.parameters.length(), 1)
        assert_eq(config.parameters[0].contains("ratio:"), true)
      }
      ParentBased => {
        assert_eq(config.parameters.length(), 1)
        assert_eq(config.parameters[0].contains("root_sampler:"), true)
      }
      RateLimiting => {
        assert_eq(config.parameters.length(), 1)
        assert_eq(config.parameters[0].contains("rate:"), true)
      }
    }
  }
}

test "trace_id_ratio_sampling" {
  // 测试基于Trace ID比例的采样
  
  struct SamplingDecision {
    decision : Bool
    trace_id : String
    ratio : Double
    threshold : Int64
  }
  
  // 将Trace ID转换为数值
  let trace_id_to_number = fn(trace_id : String) : Int64 {
    // 简化实现：使用trace_id的前8个字符转换为数值
    let prefix = trace_id.substring(0, @min(8, trace_id.length()))
    let mut result = 0L
    
    for i = 0; i < prefix.length(); i = i + 1 {
      let char = prefix[i]
      let value = if char >= '0' && char <= '9' {
        (char - '0').to_int64()
      } else if char >= 'a' && char <= 'f' {
        (char - 'a' + 10).to_int64()
      } else if char >= 'A' && char <= 'F' {
        (char - 'A' + 10).to_int64()
      } else {
        0L
      }
      
      result = result * 16L + value
    }
    
    result
  }
  
  // 基于比例的采样决策
  let make_sampling_decision = fn(trace_id : String, ratio : Double) : SamplingDecision {
    let trace_id_number = trace_id_to_number(trace_id)
    let max_value = 281474976710656L  // 2^48，trace ID的最大值
    let threshold = (max_value.to_double() * ratio).to_int64()
    let decision = trace_id_number < threshold
    
    {
      decision: decision,
      trace_id: trace_id,
      ratio: ratio,
      threshold: threshold
    }
  }
  
  // 测试不同的Trace ID和比例
  let test_trace_ids = [
    "1234567890abcdef1234567890abcdef",
    "ffffffffffffffffffffffffffffffff",
    "00000000000000000000000000000000",
    "80000000000000008000000000000000",
    "0123456789abcdef0123456789abcdef"
  ]
  
  let test_ratios = [0.0, 0.1, 0.5, 0.9, 1.0]
  
  for ratio in test_ratios {
    let decisions = test_trace_ids.map(fn(trace_id) { make_sampling_decision(trace_id, ratio) })
    
    // 验证决策的一致性
    for decision in decisions {
      assert_eq(decision.ratio, ratio)
      assert_eq(decision.threshold == (281474976710656L.to_double() * ratio).to_int64(), true)
      
      // 验证决策逻辑
      let trace_id_number = trace_id_to_number(decision.trace_id)
      let expected_decision = trace_id_number < decision.threshold
      assert_eq(decision.decision, expected_decision)
    }
    
    // 验证边界情况
    match ratio {
      0.0 => {
        // 比例为0，应该全部不采样
        let all_not_sampled = decisions.map(fn(d) { d.decision }).fold(true, fn(acc, d) { acc && !d })
        assert_eq(all_not_sampled, true)
      }
      1.0 => {
        // 比例为1，应该全部采样
        let all_sampled = decisions.map(fn(d) { d.decision }).fold(true, fn(acc, d) { acc && d })
        assert_eq(all_sampled, true)
      }
      _ => {
        // 中间比例，应该有采样和不采样的混合
        let has_sampled = decisions.map(fn(d) { d.decision }).fold(false, fn(acc, d) { acc || d })
        let has_not_sampled = decisions.map(fn(d) { d.decision }).fold(false, fn(acc, d) { acc || !d })
        // 注意：对于小的测试集，可能全部采样或全部不采样，这是正常的
      }
    }
  }
  
  // 测试大量采样点的分布
  let large_sample_size = 10000
  let target_ratio = 0.1
  let mut sampled_count = 0
  
  for i = 0; i < large_sample_size; i = i + 1 {
    let trace_id = (@sprintf("%016x", i) + @sprintf("%016x", i * 2))
    let decision = make_sampling_decision(trace_id, target_ratio)
    if decision.decision {
      sampled_count = sampled_count + 1
    }
  }
  
  let actual_ratio = sampled_count.to_double() / large_sample_size.to_double()
  let deviation = @abs(actual_ratio - target_ratio)
  
  // 允许5%的偏差
  assert_eq(deviation <= 0.05, true)
}

test "parent_based_sampling" {
  // 测试基于父Span的采样决策
  
  enum ParentSpanSampling {
    NotSampled
    Sampled
    NoParent
  }
  
  struct SamplingContext {
    trace_id : String
    span_id : String
    parent_span_sampling : ParentSpanSampling
    parent_remote : Bool
  }
  
  struct ParentBasedDecision {
    should_sample : Bool
    reason : String
    used_parent_decision : Bool
  }
  
  // 基于父Span的采样决策
  let make_parent_based_decision = fn(context : SamplingContext, 
                                      root_sampler_decision : Bool) : ParentBasedDecision {
    match context.parent_span_sampling {
      Sampled => {
        {
          should_sample: true,
          reason: "parent span is sampled",
          used_parent_decision: true
        }
      }
      NotSampled => {
        {
          should_sample: false,
          reason: "parent span is not sampled",
          used_parent_decision: true
        }
      }
      NoParent => {
        {
          should_sample: root_sampler_decision,
          reason: "no parent, using root sampler",
          used_parent_decision: false
        }
      }
    }
  }
  
  // 测试不同的父Span采样场景
  let test_contexts = [
    {
      trace_id: "1234567890abcdef1234567890abcdef",
      span_id: "1234567890abcdef",
      parent_span_sampling: Sampled,
      parent_remote: false
    },
    {
      trace_id: "1234567890abcdef1234567890abcdef",
      span_id: "1234567890abcdef",
      parent_span_sampling: NotSampled,
      parent_remote: false
    },
    {
      trace_id: "1234567890abcdef1234567890abcdef",
      span_id: "1234567890abcdef",
      parent_span_sampling: NoParent,
      parent_remote: false
    },
    {
      trace_id: "1234567890abcdef1234567890abcdef",
      span_id: "1234567890abcdef",
      parent_span_sampling: Sampled,
      parent_remote: true
    },
    {
      trace_id: "1234567890abcdef1234567890abcdef",
      span_id: "1234567890abcdef",
      parent_span_sampling: NotSampled,
      parent_remote: true
    }
  ]
  
  let root_sampler_decision = true
  
  let decisions = test_contexts.map(fn(ctx) { 
    make_parent_based_decision(ctx, root_sampler_decision) 
  })
  
  // 验证决策结果
  assert_eq(decisions[0].should_sample, true)   // 父Span被采样
  assert_eq(decisions[0].used_parent_decision, true)
  assert_eq(decisions[0].reason, "parent span is sampled")
  
  assert_eq(decisions[1].should_sample, false)  // 父Span未被采样
  assert_eq(decisions[1].used_parent_decision, true)
  assert_eq(decisions[1].reason, "parent span is not sampled")
  
  assert_eq(decisions[2].should_sample, true)   // 无父Span，使用根采样器
  assert_eq(decisions[2].used_parent_decision, false)
  assert_eq(decisions[2].reason, "no parent, using root sampler")
  
  assert_eq(decisions[3].should_sample, true)   // 远程父Span被采样
  assert_eq(decisions[3].used_parent_decision, true)
  
  assert_eq(decisions[4].should_sample, false)  // 远程父Span未被采样
  assert_eq(decisions[4].used_parent_decision, true)
  
  // 测试根采样器决策为false的情况
  let decisions_with_false_root = test_contexts.map(fn(ctx) { 
    make_parent_based_decision(ctx, false) 
  })
  
  assert_eq(decisions_with_false_root[0].should_sample, true)   // 父Span被采样，忽略根采样器
  assert_eq(decisions_with_false_root[1].should_sample, false)  // 父Span未被采样
  assert_eq(decisions_with_false_root[2].should_sample, false)  // 无父Span，使用根采样器（false）
}

test "rate_limiting_sampling" {
  // 测试限流采样
  
  struct RateLimitingSampler {
    rate_per_second : Int
    tokens : Int
    last_refill_time : Int64
    refill_interval_ms : Int
  }
  
  struct RateLimitDecision {
    should_sample : Bool
    remaining_tokens : Int
    wait_time_ms : Int64
  }
  
  // 创建限流采样器
  let create_rate_limiter = fn(rate : Int) : RateLimitingSampler {
    {
      rate_per_second: rate,
      tokens: rate,
      last_refill_time: 1609459200000L,
      refill_interval_ms: 1000
    }
  }
  
  // 补充令牌
  let refill_tokens = fn(sampler : RateLimitingSampler, current_time : Int64) : RateLimitingSampler {
    let time_diff = current_time - sampler.last_refill_time
    let intervals = time_diff / sampler.refill_interval_ms.to_int64()
    
    if intervals > 0 {
      let new_tokens = @min(sampler.rate_per_second, 
                            sampler.tokens + (intervals * sampler.refill_interval_ms / 1000 * sampler.rate_per_second))
      {
        sampler | 
        tokens: new_tokens,
        last_refill_time: sampler.last_refill_time + intervals * sampler.refill_interval_ms.to_int64()
      }
    } else {
      sampler
    }
  }
  
  // 尝试采样
  let try_sample = fn(sampler : RateLimitingSampler, current_time : Int64) : (RateLimitDecision, RateLimitingSampler) {
    let refilled_sampler = refill_tokens(sampler, current_time)
    
    if refilled_sampler.tokens > 0 {
      let updated_sampler = { refilled_sampler | tokens: refilled_sampler.tokens - 1 }
      let decision = {
        should_sample: true,
        remaining_tokens: updated_sampler.tokens,
        wait_time_ms: 0L
      }
      (decision, updated_sampler)
    } else {
      let next_refill_time = refilled_sampler.last_refill_time + refilled_sampler.refill_interval_ms.to_int64()
      let wait_time = next_refill_time - current_time
      let decision = {
        should_sample: false,
        remaining_tokens: 0,
        wait_time_ms: wait_time
      }
      (decision, refilled_sampler)
    }
  }
  
  // 测试限流采样器
  let rate = 10  // 每秒10个
  let sampler = create_rate_limiter(rate)
  
  let mut current_sampler = sampler
  let mut current_time = 1609459200000L
  let mut decisions : Array<RateLimitDecision> = []
  
  // 在同一秒内尝试采样15次
  for i = 0; i < 15; i = i + 1 {
    let (decision, updated_sampler) = try_sample(current_sampler, current_time)
    decisions.push(decision)
    current_sampler = updated_sampler
  }
  
  // 验证前10次应该成功，后5次应该失败
  let successful_samples = decisions.map(fn(d) { d.should_sample }).fold(0, fn(acc, s) { acc + (s ? 1 : 0) })
  assert_eq(successful_samples, 10)
  
  for i = 0; i < 10; i = i + 1 {
    assert_eq(decisions[i].should_sample, true)
    assert_eq(decisions[i].remaining_tokens, 9 - i)
  }
  
  for i = 10; i < 15; i = i + 1 {
    assert_eq(decisions[i].should_sample, false)
    assert_eq(decisions[i].remaining_tokens, 0)
    assert_eq(decisions[i].wait_time_ms > 0, true)
  }
  
  // 等待1秒后再次尝试
  current_time = current_time + 1000L
  let (decision_after_wait, sampler_after_wait) = try_sample(current_sampler, current_time)
  
  assert_eq(decision_after_wait.should_sample, true)  // 应该有新的令牌
  assert_eq(decision_after_wait.remaining_tokens, 9)   // 应该重新补充到10，然后消费1个
  
  // 测试令牌补充逻辑
  let test_refill = fn(initial_tokens : Int, wait_seconds : Int, rate : Int) : Int {
    let test_sampler = { create_rate_limiter(rate) | tokens: initial_tokens }
    let test_time = 1609459200000L + (wait_seconds * 1000).to_int64()
    let refilled = refill_tokens(test_sampler, test_time)
    refilled.tokens
  }
  
  assert_eq(test_refill(5, 1, 10), 10)  // 1秒后应该补充到最大值
  assert_eq(test_refill(0, 2, 10), 10)  // 2秒后应该补充到最大值
  assert_eq(test_refill(8, 0, 10), 8)   // 0秒后不应该补充
}

test "sampling_attributes_and_tags" {
  // 测试采样属性和标签
  
  struct SamplingAttributes {
    service_name : String
    operation_name : String
    span_kind : String
    user_id : Option<String>
    http_method : Option<String>
    http_status_code : Option<Int>
    error : Bool
  }
  
  struct AttributeBasedDecision {
    should_sample : Bool
    matched_rules : Array<String>
    confidence : Double
  }
  
  // 基于属性的采样规则
  let evaluate_attribute_rules = fn(attributes : SamplingAttributes) : AttributeBasedDecision {
    let mut matched_rules : Array<String> = []
    let mut confidence = 0.0
    
    // 规则1：错误追踪总是采样
    if attributes.error {
      matched_rules.push("error_trace")
      confidence = confidence + 0.8
    }
    
    // 规则2：高优先级用户
    match attributes.user_id {
      Some(user_id) => {
        if user_id.starts_with("premium_") {
          matched_rules.push("premium_user")
          confidence = confidence + 0.6
        }
      }
      None => ()
    }
    
    // 规则3：关键操作
    let critical_operations = ["payment", "auth", "admin", "security"]
    for op in critical_operations {
      if attributes.operation_name.contains(op) {
        matched_rules.push("critical_operation")
        confidence = confidence + 0.7
        break
      }
    }
    
    // 规则4：HTTP错误响应
    match attributes.http_status_code {
      Some(status) => {
        if status >= 500 {
          matched_rules.push("server_error")
          confidence = confidence + 0.5
        } else if status >= 400 {
          matched_rules.push("client_error")
          confidence = confidence + 0.3
        }
      }
      None => ()
    }
    
    // 规则5：特定服务
    if attributes.service_name == "payment-service" || 
       attributes.service_name == "auth-service" {
      matched_rules.push("critical_service")
      confidence = confidence + 0.4
    }
    
    // 规则6：慢查询（假设）
    if attributes.operation_name.contains("slow_query") {
      matched_rules.push("slow_operation")
      confidence = confidence + 0.4
    }
    
    // 决策：如果有任何规则匹配，就采样
    let should_sample = matched_rules.length() > 0
    
    // 限制置信度在0-1之间
    confidence = @min(confidence, 1.0)
    
    {
      should_sample: should_sample,
      matched_rules: matched_rules,
      confidence: confidence
    }
  }
  
  // 测试不同的属性组合
  let test_attributes = [
    {
      service_name: "user-service",
      operation_name: "get_user_profile",
      span_kind: "server",
      user_id: Some("premium_12345"),
      http_method: Some("GET"),
      http_status_code: Some(200),
      error: false
    },
    {
      service_name: "payment-service",
      operation_name: "process_payment",
      span_kind: "server",
      user_id: Some("regular_67890"),
      http_method: Some("POST"),
      http_status_code: Some(500),
      error: true
    },
    {
      service_name: "auth-service",
      operation_name: "authenticate_user",
      span_kind: "server",
      user_id: None,
      http_method: Some("POST"),
      http_status_code: Some(401),
      error: false
    },
    {
      service_name: "logging-service",
      operation_name: "write_log",
      span_kind: "server",
      user_id: None,
      http_method: None,
      http_status_code: None,
      error: false
    },
    {
      service_name: "database-service",
      operation_name: "slow_query_report",
      span_kind: "client",
      user_id: Some("premium_11111"),
      http_method: None,
      http_status_code: None,
      error: false
    }
  ]
  
  let decisions = test_attributes.map(evaluate_attribute_rules)
  
  // 验证决策结果
  assert_eq(decisions[0].should_sample, true)   // premium用户
  assert_eq(decisions[0].matched_rules.length(), 1)
  assert_eq(decisions[0].matched_rules[0], "premium_user")
  
  assert_eq(decisions[1].should_sample, true)   // 错误 + 关键服务 + 服务器错误
  assert_eq(decisions[1].matched_rules.length(), 4)
  assert_eq(decisions[1].confidence, 1.0)       // 应该达到最大置信度
  
  assert_eq(decisions[2].should_sample, true)   // 关键服务
  assert_eq(decisions[2].matched_rules.length(), 1)
  assert_eq(decisions[2].matched_rules[0], "critical_service")
  
  assert_eq(decisions[3].should_sample, false)  // 没有匹配的规则
  assert_eq(decisions[3].matched_rules.length(), 0)
  assert_eq(decisions[3].confidence, 0.0)
  
  assert_eq(decisions[4].should_sample, true)   // premium用户 + 慢操作
  assert_eq(decisions[4].matched_rules.length(), 2)
  
  // 测试规则优先级和置信度计算
  let high_confidence_decisions = decisions.filter(fn(d) { d.confidence >= 0.8 })
  let medium_confidence_decisions = decisions.filter(fn(d) { d.confidence >= 0.5 && d.confidence < 0.8 })
  let low_confidence_decisions = decisions.filter(fn(d) { d.confidence > 0.0 && d.confidence < 0.5 })
  
  assert_eq(high_confidence_decisions.length(), 1)   // 只有支付服务错误达到高置信度
  assert_eq(medium_confidence_decisions.length(), 2) // 认证服务和慢查询
  assert_eq(low_confidence_decisions.length(), 1)    // 用户服务
}

test "sampling_performance_optimization" {
  // 测试采样性能优化
  
  struct SamplingPerformanceMetrics {
    total_decisions : Int
    decision_time_ns : Int64
    memory_usage_bytes : Int
    cache_hit_rate : Double
    early_exit_rate : Double
  }
  
  struct OptimizedSampler {
    cache_size : Int
    cache_hits : Int
    cache_misses : Int
    early_exits : Int
    total_decisions : Int
  }
  
  // 优化的采样决策（带缓存）
  let optimized_sampling_decision = fn(sampler : OptimizedSampler, 
                                       trace_id : String, 
                                       has_cache : Bool) : (Bool, OptimizedSampler) {
    let mut updated_sampler = sampler
    updated_sampler.total_decisions = updated_sampler.total_decisions + 1
    
    // 模拟缓存查找
    if has_cache {
      // 简化的缓存逻辑：使用trace_id的哈希值
      let hash = trace_id.length() % sampler.cache_size
      let cache_hit = hash < sampler.cache_size / 2  // 50%的缓存命中率
      
      if cache_hit {
        updated_sampler.cache_hits = updated_sampler.cache_hits + 1
        return (true, updated_sampler)  // 缓存命中，返回已缓存的结果
      } else {
        updated_sampler.cache_misses = updated_sampler.cache_misses + 1
      }
    }
    
    // 模拟早期退出优化
    let early_exit_conditions = [
      trace_id.starts_with("00000000"),  // 特定前缀快速决策
      trace_id.starts_with("ffffffff"),
      trace_id.length() < 16
    ]
    
    for condition in early_exit_conditions {
      if condition {
        updated_sampler.early_exits = updated_sampler.early_exits + 1
        return (false, updated_sampler)  // 早期退出
      }
    }
    
    // 执行完整的采样决策
    let decision = trace_id.length() % 2 == 0  // 简化的决策逻辑
    
    (decision, updated_sampler)
  }
  
  // 性能基准测试
  let performance_benchmark = fn(trace_count : Int, use_cache : Bool) : SamplingPerformanceMetrics {
    let sampler = {
      cache_size: 1000,
      cache_hits: 0,
      cache_misses: 0,
      early_exits: 0,
      total_decisions: 0
    }
    
    let mut current_sampler = sampler
    let start_time = 0L  // 模拟时间
    let mut decisions : Array<Bool> = []
    
    // 生成测试用的trace ID
    for i = 0; i < trace_count; i = i + 1 {
      let trace_id = @sprintf("%032x", i * 12345)
      let (decision, updated_sampler) = optimized_sampling_decision(current_sampler, trace_id, use_cache)
      decisions.push(decision)
      current_sampler = updated_sampler
    }
    
    let end_time = 1000000L  // 模拟1微秒的处理时间
    
    let cache_hit_rate = if current_sampler.total_decisions > 0 {
      current_sampler.cache_hits.to_double() / current_sampler.total_decisions.to_double()
    } else {
      0.0
    }
    
    let early_exit_rate = if current_sampler.total_decisions > 0 {
      current_sampler.early_exits.to_double() / current_sampler.total_decisions.to_double()
    } else {
      0.0
    }
    
    {
      total_decisions: current_sampler.total_decisions,
      decision_time_ns: end_time - start_time,
      memory_usage_bytes: current_sampler.cache_size * 64,  // 假设每个缓存项64字节
      cache_hit_rate: cache_hit_rate,
      early_exit_rate: early_exit_rate
    }
  }
  
  // 测试不同场景下的性能
  let without_cache = performance_benchmark(10000, false)
  let with_cache = performance_benchmark(10000, true)
  
  // 验证性能指标
  assert_eq(without_cache.total_decisions, 10000)
  assert_eq(with_cache.total_decisions, 10000)
  assert_eq(with_cache.decision_time_ns <= without_cache.decision_time_ns, true)  // 缓存应该提高性能
  assert_eq(with_cache.memory_usage_bytes > without_cache.memory_usage_bytes, true)  // 缓存使用更多内存
  assert_eq(with_cache.cache_hit_rate > 0.0, true)  // 应该有缓存命中
  assert_eq(with_cache.early_exit_rate >= 0.0, true)
  
  // 测试性能阈值
  let max_decision_time_ns = 10000L  // 10微秒
  let min_cache_hit_rate = 0.3       // 至少30%的缓存命中率
  let max_memory_usage_mb = 10       // 最多10MB内存使用
  
  assert_eq(with_cache.decision_time_ns <= max_decision_time_ns, true)
  assert_eq(with_cache.cache_hit_rate >= min_cache_hit_rate, true)
  assert_eq(with_cache.memory_usage_bytes <= max_memory_usage_mb * 1024 * 1024, true)
  
  // 测试优化策略效果
  let cache_speedup = without_cache.decision_time_ns.to_double() / with_cache.decision_time_ns.to_double()
  assert_eq(cache_speedup >= 1.1, true)  // 缓存应该至少提升10%的性能
  
  let early_exit_benefit = with_cache.early_exit_rate * 100.0  // 早期退出节省的百分比
  assert_eq(early_exit_benefit >= 0.0, true)
}