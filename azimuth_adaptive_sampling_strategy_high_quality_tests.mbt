// Azimuth 自适应采样策略测试
// 专注于测试自适应采样算法的智能调整和优化能力

// 测试1: 基于负载的自适应采样
test "基于负载的自适应采样" {
  // 定义系统负载指标
  type SystemLoad = {
    cpu_usage: Float,
    memory_usage: Float,
    network_throughput: Float,
    disk_io: Float,
    active_spans: Int,
    queue_size: Int
  }
  
  // 定义采样决策
  type SamplingDecision = {
    should_sample: Bool,
    sampling_rate: Float,
    reason: String,
    confidence: Float
  }
  
  // 定义负载阈值
  type LoadThresholds = {
    cpu_high: Float,
    cpu_critical: Float,
    memory_high: Float,
    memory_critical: Float,
    throughput_high: Float,
    throughput_critical: Float,
    queue_size_high: Int,
    queue_size_critical: Int
  }
  
  // 创建基于负载的自适应采样器
  let create_load_based_sampler = fn(thresholds: LoadThresholds) {
    let mut current_sampling_rate = 0.1  // 初始采样率10%
    let mut adjustment_history = []
    let mut last_adjustment_time = Time::now()
    let adjustment_cooldown = 5 * 60 * 1000  // 5分钟调整冷却时间
    
    {
      current_sampling_rate,
      adjustment_history,
      
      // 评估系统负载
      evaluate_load: fn(load: SystemLoad) {
        let load_score = 0.0
        let mut reasons = []
        
        // CPU负载评估
        if load.cpu_usage > thresholds.cpu_critical {
          load_score = load_score + 0.4
          reasons = reasons.push("critical_cpu")
        } else if load.cpu_usage > thresholds.cpu_high {
          load_score = load_score + 0.2
          reasons = reasons.push("high_cpu")
        }
        
        // 内存负载评估
        if load.memory_usage > thresholds.memory_critical {
          load_score = load_score + 0.4
          reasons = reasons.push("critical_memory")
        } else if load.memory_usage > thresholds.memory_high {
          load_score = load_score + 0.2
          reasons = reasons.push("high_memory")
        }
        
        // 网络吞吐量评估
        if load.network_throughput > thresholds.throughput_critical {
          load_score = load_score + 0.3
          reasons = reasons.push("critical_throughput")
        } else if load.network_throughput > thresholds.throughput_high {
          load_score = load_score + 0.15
          reasons = reasons.push("high_throughput")
        }
        
        // 队列大小评估
        if load.queue_size > thresholds.queue_size_critical {
          load_score = load_score + 0.3
          reasons = reasons.push("critical_queue")
        } else if load.queue_size > thresholds.queue_size_high {
          load_score = load_score + 0.15
          reasons = reasons.push("high_queue")
        }
        
        // 活跃Span数量评估
        if load.active_spans > 10000 {
          load_score = load_score + 0.2
          reasons = reasons.push("high_active_spans")
        } else if load.active_spans > 5000 {
          load_score = load_score + 0.1
          reasons = reasons.push("moderate_active_spans")
        }
        
        {
          load_score: Float::min(load_score, 1.0),
          reasons: reasons
        }
      },
      
      // 调整采样率
      adjust_sampling_rate: fn(load_evaluation) {
        let current_time = Time::now()
        
        // 检查是否在冷却期内
        if current_time - last_adjustment_time < adjustment_cooldown {
          return current_sampling_rate
        }
        
        let old_rate = current_sampling_rate
        let mut new_rate = current_sampling_rate
        
        // 根据负载评分调整采样率
        if load_evaluation.load_score > 0.7 {
          // 高负载：降低采样率
          new_rate = Float::max(0.01, current_sampling_rate * 0.7)  // 最多降低到1%
        } else if load_evaluation.load_score > 0.4 {
          // 中等负载：适度降低采样率
          new_rate = Float::max(0.05, current_sampling_rate * 0.85)  // 最多降低到5%
        } else if load_evaluation.load_score < 0.1 {
          // 低负载：提高采样率
          new_rate = Float::min(0.5, current_sampling_rate * 1.2)  // 最多提高到50%
        }
        
        // 记录调整历史
        if new_rate != old_rate {
          let adjustment = {
            timestamp: current_time,
            old_rate: old_rate,
            new_rate: new_rate,
            load_score: load_evaluation.load_score,
            reasons: load_evaluation.reasons
          }
          
          adjustment_history = adjustment_history.push(adjustment)
          last_adjustment_time = current_time
          current_sampling_rate = new_rate
        }
        
        current_sampling_rate
      },
      
      // 采样决策
      should_sample: fn(trace_data: String, load: SystemLoad) {
        let load_evaluation = this.evaluate_load(load)
        let adjusted_rate = this.adjust_sampling_rate(load_evaluation)
        
        // 基于调整后的采样率做决策
        let random_value = Random::float()
        let should_sample = random_value < adjusted_rate
        
        let confidence = if load_evaluation.load_score > 0.5 {
          0.9  // 高负载时更有信心
        } else if load_evaluation.load_score < 0.2 {
          0.7  // 低负载时信心较低
        } else {
          0.8  // 中等负载时中等信心
        }
        
        let reason = if should_sample {
          "sampled_at_rate_" + (adjusted_rate * 100.0).to_string() + "%"
        } else {
          "dropped_due_to_load_" + (load_evaluation.load_score * 100.0).to_string() + "%"
        }
        
        {
          should_sample,
          sampling_rate: adjusted_rate,
          reason,
          confidence
        }
      },
      
      // 获取当前采样率
      get_current_rate: fn() {
        current_sampling_rate
      },
      
      // 获取调整历史
      get_adjustment_history: fn() {
        adjustment_history
      }
    }
  }
  
  // 创建测试负载阈值
  let thresholds = {
    cpu_high: 70.0,
    cpu_critical: 90.0,
    memory_high: 75.0,
    memory_critical: 90.0,
    throughput_high: 80.0,
    throughput_critical: 95.0,
    queue_size_high: 1000,
    queue_size_critical: 5000
  }
  
  // 创建负载采样器
  let load_sampler = create_load_based_sampler(thresholds)
  
  // 测试低负载情况
  let low_load = {
    cpu_usage: 30.0,
    memory_usage: 40.0,
    network_throughput: 25.0,
    disk_io: 20.0,
    active_spans: 1000,
    queue_size: 100
  }
  
  let low_load_evaluation = load_sampler.evaluate_load(low_load)
  assert_true(low_load_evaluation.load_score < 0.2)
  assert_true(low_load_evaluation.reasons.length() == 0)
  
  // 在低负载下，采样率应该提高
  let initial_rate = load_sampler.get_current_rate()
  let adjusted_rate_low = load_sampler.adjust_sampling_rate(low_load_evaluation)
  assert_true(adjusted_rate_low >= initial_rate)
  
  // 测试低负载下的采样决策
  let low_load_decisions = []
  for i in 0..100 {
    let decision = load_sampler.should_sample("test-trace-" + i.to_string(), low_load)
    low_load_decisions = low_load_decisions.push(decision.should_sample)
  }
  
  let low_load_sampled_count = low_load_decisions.filter(fn(sampled) { sampled }).length()
  let low_load_actual_rate = (low_load_sampled_count as Float) / 100.0
  assert_true(low_load_actual_rate > initial_rate * 0.8)  // 实际采样率应该接近调整后的采样率
  
  // 测试中等负载情况
  let medium_load = {
    cpu_usage: 75.0,
    memory_usage: 78.0,
    network_throughput: 60.0,
    disk_io: 50.0,
    active_spans: 6000,
    queue_size: 800
  }
  
  let medium_load_evaluation = load_sampler.evaluate_load(medium_load)
  assert_true(medium_load_evaluation.load_score > 0.2)
  assert_true(medium_load_evaluation.load_score < 0.7)
  assert_true(medium_load_evaluation.reasons.length() > 0)
  
  // 在中等负载下，采样率应该适度降低
  let rate_before_medium = load_sampler.get_current_rate()
  let adjusted_rate_medium = load_sampler.adjust_sampling_rate(medium_load_evaluation)
  assert_true(adjusted_rate_medium <= rate_before_medium)
  
  // 测试高负载情况
  let high_load = {
    cpu_usage: 92.0,
    memory_usage: 94.0,
    network_throughput: 97.0,
    disk_io: 85.0,
    active_spans: 12000,
    queue_size: 6000
  }
  
  let high_load_evaluation = load_sampler.evaluate_load(high_load)
  assert_true(high_load_evaluation.load_score > 0.7)
  assert_true(high_load_evaluation.reasons.contains("critical_cpu"))
  assert_true(high_load_evaluation.reasons.contains("critical_memory"))
  
  // 在高负载下，采样率应该大幅降低
  let rate_before_high = load_sampler.get_current_rate()
  let adjusted_rate_high = load_sampler.adjust_sampling_rate(high_load_evaluation)
  assert_true(adjusted_rate_high < rate_before_high)
  assert_true(adjusted_rate_high <= 0.1)  // 应该降低到10%以下
  
  // 测试高负载下的采样决策
  let high_load_decisions = []
  for i in 0..100 {
    let decision = load_sampler.should_sample("test-trace-" + i.to_string(), high_load)
    high_load_decisions = high_load_decisions.push(decision.should_sample)
  }
  
  let high_load_sampled_count = high_load_decisions.filter(fn(sampled) { sampled }).length()
  let high_load_actual_rate = (high_load_sampled_count as Float) / 100.0
  assert_true(high_load_actual_rate < 0.15)  // 实际采样率应该很低
  
  // 验证调整历史
  let adjustment_history = load_sampler.get_adjustment_history()
  assert_true(adjustment_history.length() > 0)
  
  for adjustment in adjustment_history {
    assert_true(adjustment.timestamp > 0)
    assert_true(adjustment.old_rate >= 0.0)
    assert_true(adjustment.new_rate >= 0.0)
    assert_true(adjustment.new_rate <= 1.0)
    assert_true(adjustment.load_score >= 0.0)
    assert_true(adjustment.load_score <= 1.0)
    assert_true(adjustment.reasons.length() > 0)
  }
}

// 测试2: 基于业务重要性的自适应采样
test "基于业务重要性的自适应采样" {
  // 定义业务重要性指标
  type BusinessImportance = {
    service_criticality: String,  // "critical", "important", "normal", "low"
    user_impact: String,         // "high", "medium", "low"
    revenue_impact: Float,       // 0.0 to 1.0
    error_rate: Float,           // 0.0 to 1.0
    response_time_sla: Float,    // 目标响应时间（毫秒）
    customer_tier: String        // "enterprise", "premium", "standard", "free"
  }
  
  // 定义重要性权重
  type ImportanceWeights = {
    service_weights: Map[String, Float],
    user_impact_weights: Map[String, Float],
    customer_tier_weights: Map[String, Float],
    error_rate_multiplier: Float,
    response_time_multiplier: Float
  }
  
  // 创建基于业务重要性的采样器
  let create_business_importance_sampler = fn(weights: ImportanceWeights) {
    let mut sampling_decisions = Map::empty()  // trace_id -> decision
    let mut importance_scores = Map::empty()   // trace_id -> score
    
    {
      sampling_decisions,
      importance_scores,
      
      // 计算业务重要性分数
      calculate_importance_score: fn(importance: BusinessImportance, trace_attributes: Map[String, String]) {
        let mut score = 0.0
        
        // 服务关键性权重
        let service_weight = match Map::get(weights.service_weights, importance.service_criticality) {
          Some(weight) => weight
          None => 0.5
        }
        score = score + service_weight * 0.3
        
        // 用户影响权重
        let user_impact_weight = match Map::get(weights.user_impact_weights, importance.user_impact) {
          Some(weight) => weight
          None => 0.5
        }
        score = score + user_impact_weight * 0.2
        
        // 客户层级权重
        let customer_weight = match Map::get(weights.customer_tier_weights, importance.customer_tier) {
          Some(weight) => weight
          None => 0.5
        }
        score = score + customer_weight * 0.2
        
        // 收入影响
        score = score + importance.revenue_impact * 0.15
        
        // 错误率影响（错误率越高，重要性越高）
        score = score + importance.error_rate * weights.error_rate_multiplier * 0.1
        
        // 基于特定属性的额外权重
        match Map::get(trace_attributes, "http.status_code") {
          Some(status_code) => {
            if status_code.to_int() >= 500 {
              score = score + 0.3  // 服务器错误增加重要性
            } else if status_code.to_int() >= 400 {
              score = score + 0.1  // 客户端错误适度增加重要性
            }
          }
          None => ()
        }
        
        match Map::get(trace_attributes, "error.type") {
          Some(_) => {
            score = score + 0.4  // 有错误类型标记的trace更重要
          }
          None => ()
        }
        
        match Map::get(trace_attributes, "user.premium") {
          Some(premium) => {
            if premium == "true" {
              score = score + 0.2  // 高级用户更重要
            }
          }
          None => ()
        }
        
        Float::min(score, 1.0)  // 确保分数不超过1.0
      },
      
      // 基于重要性分数的采样决策
      make_sampling_decision: fn(trace_id: String, importance_score: Float, base_rate: Float) {
        // 根据重要性分数调整采样率
        let adjusted_rate = if importance_score > 0.8 {
          // 高重要性：总是采样
          1.0
        } else if importance_score > 0.6 {
          // 中高重要性：提高采样率
          Float::min(1.0, base_rate * 3.0)
        } else if importance_score > 0.4 {
          // 中等重要性：使用基础采样率
          base_rate
        } else if importance_score > 0.2 {
          // 中低重要性：降低采样率
          base_rate * 0.5
        } else {
          // 低重要性：大幅降低采样率
          base_rate * 0.1
        }
        
        let random_value = Random::float()
        let should_sample = random_value < adjusted_rate
        
        let decision = {
          should_sample,
          sampling_rate: adjusted_rate,
          importance_score: importance_score
        }
        
        // 记录决策
        sampling_decisions = Map::insert(sampling_decisions, trace_id, decision)
        importance_scores = Map::insert(importance_scores, trace_id, importance_score)
        
        decision
      },
      
      // 获取采样决策
      get_decision: fn(trace_id: String) {
        Map::get(sampling_decisions, trace_id)
      },
      
      // 获取重要性分数
      get_importance_score: fn(trace_id: String) {
        Map::get(importance_scores, trace_id)
      },
      
      // 获取采样统计
      get_sampling_stats: fn() {
        let decisions = sampling_decisions.map(fn(kv) { kv.1 })
        let total_decisions = decisions.length()
        
        if total_decisions == 0 {
          return {
            total_traces: 0,
            sampled_traces: 0,
            sampling_rate: 0.0,
            avg_importance_score: 0.0
          }
        }
        
        let sampled_count = decisions.filter(fn(decision) { decision.should_sample }).length()
        let sampling_rate = (sampled_count as Float) / (total_decisions as Float)
        
        let total_importance = decisions.reduce(0.0, fn(acc, decision) { 
          acc + decision.importance_score 
        })
        let avg_importance_score = total_importance / (total_decisions as Float)
        
        {
          total_traces: total_decisions,
          sampled_traces: sampled_count,
          sampling_rate: sampling_rate,
          avg_importance_score: avg_importance_score
        }
      }
    }
  }
  
  // 创建业务重要性权重
  let weights = {
    service_weights: [
      ("critical", 1.0),
      ("important", 0.8),
      ("normal", 0.5),
      ("low", 0.2)
    ],
    user_impact_weights: [
      ("high", 1.0),
      ("medium", 0.6),
      ("low", 0.3)
    ],
    customer_tier_weights: [
      ("enterprise", 1.0),
      ("premium", 0.8),
      ("standard", 0.5),
      ("free", 0.2)
    ],
    error_rate_multiplier: 2.0,
    response_time_multiplier: 1.5
  }
  
  // 创建业务重要性采样器
  let business_sampler = create_business_importance_sampler(weights)
  
  // 测试不同重要性的trace
  let critical_trace_importance = {
    service_criticality: "critical",
    user_impact: "high",
    revenue_impact: 0.9,
    error_rate: 0.05,
    response_time_sla: 100.0,
    customer_tier: "enterprise"
  }
  
  let critical_trace_attributes = [
    ("http.status_code", "500"),
    ("error.type", "database_error"),
    ("user.premium", "true")
  ]
  
  let critical_score = business_sampler.calculate_importance_score(critical_trace_importance, critical_trace_attributes)
  assert_true(critical_score > 0.8)
  
  let critical_decision = business_sampler.make_sampling_decision("critical-trace-1", critical_score, 0.1)
  assert_true(critical_decision.should_sample)
  assert_eq(critical_decision.sampling_rate, 1.0)
  
  // 测试中等重要性的trace
  let normal_trace_importance = {
    service_criticality: "normal",
    user_impact: "medium",
    revenue_impact: 0.4,
    error_rate: 0.01,
    response_time_sla: 500.0,
    customer_tier: "standard"
  }
  
  let normal_trace_attributes = [
    ("http.status_code", "200"),
    ("operation.name", "get_user_profile")
  ]
  
  let normal_score = business_sampler.calculate_importance_score(normal_trace_importance, normal_trace_attributes)
  assert_true(normal_score > 0.3)
  assert_true(normal_score < 0.7)
  
  let normal_decision = business_sampler.make_sampling_decision("normal-trace-1", normal_score, 0.1)
  // 采样决策取决于随机数，但采样率应该是基础率
  assert_eq(normal_decision.sampling_rate, 0.1)
  
  // 测试低重要性的trace
  let low_trace_importance = {
    service_criticality: "low",
    user_impact: "low",
    revenue_impact: 0.1,
    error_rate: 0.001,
    response_time_sla: 2000.0,
    customer_tier: "free"
  }
  
  let low_trace_attributes = [
    ("http.status_code", "200"),
    ("operation.name", "health_check")
  ]
  
  let low_score = business_sampler.calculate_importance_score(low_trace_importance, low_trace_attributes)
  assert_true(low_score < 0.3)
  
  let low_decision = business_sampler.make_sampling_decision("low-trace-1", low_score, 0.1)
  assert_eq(low_decision.sampling_rate, 0.01)  // 基础采样率的10%
  
  // 测试批量采样决策
  let batch_traces = []
  
  // 添加不同重要性的trace
  for i in 0..20 {
    let (importance, attributes) = if i % 4 == 0 {
      (critical_trace_importance, critical_trace_attributes)
    } else if i % 4 == 1 {
      (normal_trace_importance, normal_trace_attributes)
    } else if i % 4 == 2 {
      (low_trace_importance, low_trace_attributes)
    } else {
      // 中等重要性
      ({
        service_criticality: "important",
        user_impact: "medium",
        revenue_impact: 0.6,
        error_rate: 0.02,
        response_time_sla: 300.0,
        customer_tier: "premium"
      }, [
        ("http.status_code", "200"),
        ("operation.name", "process_payment")
      ])
    }
    
    let score = business_sampler.calculate_importance_score(importance, attributes)
    business_sampler.make_sampling_decision("batch-trace-" + i.to_string(), score, 0.1)
    batch_traces = batch_traces.push(("batch-trace-" + i.to_string(), score))
  }
  
  // 验证采样统计
  let sampling_stats = business_sampler.get_sampling_stats()
  assert_eq(sampling_stats.total_traces, 20)
  assert_true(sampling_stats.sampled_traces > 0)
  assert_true(sampling_stats.sampling_rate > 0.0)
  assert_true(sampling_stats.avg_importance_score > 0.2)
  
  // 验证高重要性trace的采样率更高
  let critical_trace_count = batch_traces.filter(fn(trace) { trace.1 > 0.8 }).length()
  let critical_sampled_count = 0
  
  for (trace_id, score) in batch_traces {
    if score > 0.8 {
      match business_sampler.get_decision(trace_id) {
        Some(decision) => {
          if decision.should_sample {
            critical_sampled_count = critical_sampled_count + 1
          }
        }
        None => assert_true(false)
      }
    }
  }
  
  // 高重要性trace应该大部分被采样
  assert_true((critical_sampled_count as Float) / (critical_trace_count as Float) > 0.8)
}

// 测试3: 混合自适应采样策略
test "混合自适应采样策略" {
  // 定义混合采样策略配置
  type HybridSamplingConfig = {
    load_weight: Float,           // 负载权重
    business_weight: Float,      // 业务重要性权重
    error_weight: Float,         // 错误率权重
    latency_weight: Float,       // 延迟权重
    base_sampling_rate: Float,   // 基础采样率
    max_sampling_rate: Float,    // 最大采样率
    min_sampling_rate: Float     // 最小采样率
  }
  
  // 定义综合评估结果
  type ComprehensiveEvaluation = {
    load_factor: Float,
    business_importance: Float,
    error_factor: Float,
    latency_factor: Float,
    combined_score: Float,
    recommended_rate: Float
  }
  
  // 创建混合自适应采样器
  let create_hybrid_sampler = fn(config: HybridSamplingConfig) {
    let load_sampler = create_load_based_sampler({
      cpu_high: 70.0,
      cpu_critical: 90.0,
      memory_high: 75.0,
      memory_critical: 90.0,
      throughput_high: 80.0,
      throughput_critical: 95.0,
      queue_size_high: 1000,
      queue_size_critical: 5000
    })
    
    let business_sampler = create_business_importance_sampler({
      service_weights: [
        ("critical", 1.0),
        ("important", 0.8),
        ("normal", 0.5),
        ("low", 0.2)
      ],
      user_impact_weights: [
        ("high", 1.0),
        ("medium", 0.6),
        ("low", 0.3)
      ],
      customer_tier_weights: [
        ("enterprise", 1.0),
        ("premium", 0.8),
        ("standard", 0.5),
        ("free", 0.2)
      ],
      error_rate_multiplier: 2.0,
      response_time_multiplier: 1.5
    })
    
    let mut sampling_history = []
    
    {
      config,
      sampling_history,
      
      // 综合评估
      evaluate_comprehensive: fn(
        load: SystemLoad, 
        business_importance: BusinessImportance, 
        trace_attributes: Map[String, String],
        trace_metrics: Map[String, Float]
      ) {
        // 评估负载因子
        let load_evaluation = load_sampler.evaluate_load(load)
        let load_factor = 1.0 - load_evaluation.load_score  // 负载越高，因子越低
        
        // 评估业务重要性
        let business_score = business_sampler.calculate_importance_score(business_importance, trace_attributes)
        let business_importance = business_score
        
        // 评估错误因子
        let error_factor = match Map::get(trace_metrics, "error_rate") {
          Some(rate) => Float::min(rate * 3.0, 1.0)  // 错误率越高，因子越高
          None => 0.0
        }
        
        // 评估延迟因子
        let latency_factor = match Map::get(trace_metrics, "duration_ms") {
          Some(duration) => {
            let sla = business_importance.response_time_sla
            if duration > sla * 2.0 {
              1.0  // 超过SLA 2倍，最高优先级
            } else if duration > sla {
              (duration - sla) / sla  // 超过SLA，按比例计算
            } else {
              0.0  // 未超过SLA
            }
          }
          None => 0.0
        }
        
        // 计算综合分数
        let combined_score = 
          (load_factor * config.load_weight) +
          (business_importance * config.business_weight) +
          (error_factor * config.error_weight) +
          (latency_factor * config.latency_weight)
        
        // 计算推荐采样率
        let recommended_rate = config.base_sampling_rate + 
          (combined_score * (config.max_sampling_rate - config.base_sampling_rate))
        
        let final_rate = Float::max(config.min_sampling_rate, 
          Float::min(config.max_sampling_rate, recommended_rate))
        
        {
          load_factor,
          business_importance,
          error_factor,
          latency_factor,
          combined_score,
          recommended_rate: final_rate
        }
      },
      
      // 混合采样决策
      make_hybrid_decision: fn(
        trace_id: String,
        load: SystemLoad,
        business_importance: BusinessImportance,
        trace_attributes: Map[String, String],
        trace_metrics: Map[String, Float]
      ) {
        let evaluation = this.evaluate_comprehensive(load, business_importance, trace_attributes, trace_metrics)
        
        // 基于综合评分做采样决策
        let random_value = Random::float()
        let should_sample = random_value < evaluation.recommended_rate
        
        let decision = {
          trace_id: trace_id,
          should_sample,
          sampling_rate: evaluation.recommended_rate,
          evaluation: evaluation,
          timestamp: Time::now()
        }
        
        // 记录决策历史
        sampling_history = sampling_history.push(decision)
        
        decision
      },
      
      // 获取采样历史
      get_sampling_history: fn() {
        sampling_history
      },
      
      // 分析采样效果
      analyze_sampling_effectiveness: fn() {
        if sampling_history.length() == 0 {
          return None
        }
        
        let total_decisions = sampling_history.length()
        let sampled_decisions = sampling_history.filter(fn(d) { d.should_sample })
        let sampled_count = sampled_decisions.length()
        
        // 按重要性分组分析
        let high_importance_sampled = sampled_decisions.filter(fn(d) { 
          d.evaluation.business_importance > 0.7 
        }).length()
        
        let high_importance_total = sampling_history.filter(fn(d) { 
          d.evaluation.business_importance > 0.7 
        }).length()
        
        let high_importance_rate = if high_importance_total > 0 {
          (high_importance_sampled as Float) / (high_importance_total as Float)
        } else {
          0.0
        }
        
        // 按负载分组分析
        let high_load_sampled = sampled_decisions.filter(fn(d) { 
          d.evaluation.load_factor < 0.3  // 高负载时因子较低
        }).length()
        
        let high_load_total = sampling_history.filter(fn(d) { 
          d.evaluation.load_factor < 0.3 
        }).length()
        
        let high_load_rate = if high_load_total > 0 {
          (high_load_sampled as Float) / (high_load_total as Float)
        } else {
          0.0
        }
        
        // 计算平均综合分数
        let total_combined_score = sampling_history.reduce(0.0, fn(acc, d) { 
          acc + d.evaluation.combined_score 
        })
        let avg_combined_score = total_combined_score / (total_decisions as Float)
        
        Some({
          total_traces: total_decisions,
          sampled_traces: sampled_count,
          overall_sampling_rate: (sampled_count as Float) / (total_decisions as Float),
          high_importance_sampling_rate: high_importance_rate,
          high_load_sampling_rate: high_load_rate,
          avg_combined_score: avg_combined_score
        })
      }
    }
  }
  
  // 创建混合采样配置
  let hybrid_config = {
    load_weight: 0.3,
    business_weight: 0.4,
    error_weight: 0.2,
    latency_weight: 0.1,
    base_sampling_rate: 0.1,
    max_sampling_rate: 0.8,
    min_sampling_rate: 0.01
  }
  
  // 创建混合采样器
  let hybrid_sampler = create_hybrid_sampler(hybrid_config)
  
  // 测试不同场景的混合采样
  
  // 场景1：高负载但高重要性
  let high_load = {
    cpu_usage: 85.0,
    memory_usage: 88.0,
    network_throughput: 90.0,
    disk_io: 75.0,
    active_spans: 8000,
    queue_size: 3000
  }
  
  let high_importance = {
    service_criticality: "critical",
    user_impact: "high",
    revenue_impact: 0.9,
    error_rate: 0.1,
    response_time_sla: 100.0,
    customer_tier: "enterprise"
  }
  
  let high_importance_attrs = [
    ("http.status_code", "500"),
    ("error.type", "database_error")
  ]
  
  let high_importance_metrics = [
    ("error_rate", 0.1),
    ("duration_ms", 1500.0)
  ]
  
  let decision1 = hybrid_sampler.make_hybrid_decision(
    "trace-1", 
    high_load, 
    high_importance, 
    high_importance_attrs, 
    high_importance_metrics
  )
  
  // 即使负载高，但重要性也很高，所以采样率应该适中
  assert_true(decision1.sampling_rate > 0.05)
  assert_true(decision1.sampling_rate < 0.5)
  assert_eq(decision1.evaluation.load_factor, 1.0 - 0.85)  // 负载因子
  assert_true(decision1.evaluation.business_importance > 0.8)
  assert_true(decision1.evaluation.error_factor > 0.2)
  assert_true(decision1.evaluation.latency_factor > 0.5)
  
  // 场景2：低负载但低重要性
  let low_load = {
    cpu_usage: 25.0,
    memory_usage: 30.0,
    network_throughput: 20.0,
    disk_io: 15.0,
    active_spans: 500,
    queue_size: 50
  }
  
  let low_importance = {
    service_criticality: "low",
    user_impact: "low",
    revenue_impact: 0.1,
    error_rate: 0.001,
    response_time_sla: 2000.0,
    customer_tier: "free"
  }
  
  let low_importance_attrs = [
    ("http.status_code", "200"),
    ("operation.name", "health_check")
  ]
  
  let low_importance_metrics = [
    ("error_rate", 0.001),
    ("duration_ms", 50.0)
  ]
  
  let decision2 = hybrid_sampler.make_hybrid_decision(
    "trace-2", 
    low_load, 
    low_importance, 
    low_importance_attrs, 
    low_importance_metrics
  )
  
  // 负载低但重要性也低，采样率应该较低
  assert_true(decision2.sampling_rate < 0.2)
  assert_true(decision2.evaluation.load_factor > 0.7)
  assert_true(decision2.evaluation.business_importance < 0.3)
  assert_true(decision2.evaluation.error_factor < 0.1)
  assert_eq(decision2.evaluation.latency_factor, 0.0)
  
  // 场景3：中等负载和中等重要性，但有高延迟
  let medium_load = {
    cpu_usage: 60.0,
    memory_usage: 65.0,
    network_throughput: 55.0,
    disk_io: 50.0,
    active_spans: 3000,
    queue_size: 800
  }
  
  let medium_importance = {
    service_criticality: "normal",
    user_impact: "medium",
    revenue_impact: 0.5,
    error_rate: 0.02,
    response_time_sla: 300.0,
    customer_tier: "standard"
  }
  
  let medium_importance_attrs = [
    ("http.status_code", "200"),
    ("operation.name", "process_order")
  ]
  
  let medium_importance_metrics = [
    ("error_rate", 0.02),
    ("duration_ms", 800.0)  // 超过SLA
  ]
  
  let decision3 = hybrid_sampler.make_hybrid_decision(
    "trace-3", 
    medium_load, 
    medium_importance, 
    medium_importance_attrs, 
    medium_importance_metrics
  )
  
  // 中等负载和重要性，但延迟高，采样率应该适中
  assert_true(decision3.sampling_rate > 0.1)
  assert_true(decision3.sampling_rate < 0.4)
  assert_eq(decision3.evaluation.latency_factor, (800.0 - 300.0) / 300.0)
  
  // 批量测试混合采样
  for i in 0..50 {
    let (load, importance, attrs, metrics) = if i % 3 == 0 {
      (high_load, high_importance, high_importance_attrs, high_importance_metrics)
    } else if i % 3 == 1 {
      (low_load, low_importance, low_importance_attrs, low_importance_metrics)
    } else {
      (medium_load, medium_importance, medium_importance_attrs, medium_importance_metrics)
    }
    
    hybrid_sampler.make_hybrid_decision(
      "batch-trace-" + i.to_string(), 
      load, 
      importance, 
      attrs, 
      metrics
    )
  }
  
  // 分析采样效果
  let effectiveness = hybrid_sampler.analyze_sampling_effectiveness()
  match effectiveness {
    Some(stats) => {
      assert_eq(stats.total_traces, 50)
      assert_true(stats.sampled_traces > 0)
      assert_true(stats.overall_sampling_rate > 0.0)
      assert_true(stats.high_importance_sampling_rate > stats.overall_sampling_rate)
      assert_true(stats.high_load_sampling_rate < stats.overall_sampling_rate)
      assert_true(stats.avg_combined_score > 0.0)
    }
    None => assert_true(false)
  }
  
  // 验证采样历史
  let sampling_history = hybrid_sampler.get_sampling_history()
  assert_eq(sampling_history.length(), 50)
  
  for decision in sampling_history {
    assert_true(decision.trace_id.length() > 0)
    assert_true(decision.sampling_rate >= hybrid_config.min_sampling_rate)
    assert_true(decision.sampling_rate <= hybrid_config.max_sampling_rate)
    assert_true(decision.evaluation.combined_score >= 0.0)
    assert_true(decision.evaluation.combined_score <= 1.0)
    assert_true(decision.timestamp > 0)
  }
}