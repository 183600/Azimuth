// Azimuth 自适应采样策略测试用例
// 专注于动态采样率调整、智能采样决策、采样效果评估等功能

// 测试1: 动态采样率调整策略
test "动态采样率调整策略功能" {
  // 定义采样率调整触发器
  enum SamplingTrigger {
    ThroughputThreshold    // 吞吐量阈值触发
    ErrorRateThreshold     // 错误率阈值触发
    ResourceUsage          // 资源使用率触发
    TimeWindow             // 时间窗口触发
    ManualOverride         // 手动覆盖触发
  }
  
  // 定义采样方向
  enum SamplingDirection {
    Increase    // 增加采样率
    Decrease    // 减少采样率
    Maintain    // 保持采样率
  }
  
  // 定义采样率调整策略
  type SamplingAdjustmentStrategy = {
    name: String
    trigger: SamplingTrigger
    trigger_conditions: Map[String, Float]
    adjustment_factor: Float
    min_sampling_rate: Float
    max_sampling_rate: Float
    cooldown_period_ms: Int
  }
  
  // 定义采样状态
  type SamplingState = {
    current_rate: Float
    last_adjustment_time: Int
    adjustment_count: Int
    total_samples_collected: Int
    total_events_processed: Int
    effective_sampling_rate: Float
  }
  
  // 定义系统指标
  type SystemMetrics = {
    throughput_events_per_sec: Float
    error_rate_percent: Float
    cpu_usage_percent: Float
    memory_usage_percent: Float
    disk_io_mb_per_sec: Float
    network_io_mb_per_sec: Float
  }
  
  // 检查触发条件
  let check_trigger_conditions = fn(trigger: SamplingTrigger, conditions: Map[String, Float], metrics: SystemMetrics) -> Bool {
    match trigger {
      SamplingTrigger::ThroughputThreshold => {
        match Map::get(conditions, "max_throughput") {
          Some(max_throughput) => metrics.throughput_events_per_sec > max_throughput,
          None => false
        }
      },
      SamplingTrigger::ErrorRateThreshold => {
        match Map::get(conditions, "error_rate_threshold") {
          Some(threshold) => metrics.error_rate_percent > threshold,
          None => false
        }
      },
      SamplingTrigger::ResourceUsage => {
        let cpu_threshold = match Map::get(conditions, "cpu_threshold") {
          Some(threshold) => threshold,
          None => 80.0
        }
        
        let memory_threshold = match Map::get(conditions, "memory_threshold") {
          Some(threshold) => threshold,
          None => 80.0
        }
        
        metrics.cpu_usage_percent > cpu_threshold || metrics.memory_usage_percent > memory_threshold
      },
      SamplingTrigger::TimeWindow => {
        // 时间窗口触发通常由外部控制，这里简化为总是返回false
        false
      },
      SamplingTrigger::ManualOverride => {
        // 手动覆盖触发由外部控制，这里简化为总是返回false
        false
      }
    }
  }
  
  // 计算调整方向
  let calculate_adjustment_direction = fn(trigger: SamplingTrigger, metrics: SystemMetrics, current_rate: Float) -> SamplingDirection {
    match trigger {
      SamplingTrigger::ThroughputThreshold => {
        // 高吞吐量时减少采样率
        if metrics.throughput_events_per_sec > 1000.0 {
          SamplingDirection::Decrease
        } else if metrics.throughput_events_per_sec < 100.0 {
          SamplingDirection::Increase
        } else {
          SamplingDirection::Maintain
        }
      },
      SamplingTrigger::ErrorRateThreshold => {
        // 高错误率时增加采样率以获取更多错误信息
        if metrics.error_rate_percent > 5.0 {
          SamplingDirection::Increase
        } else {
          SamplingDirection::Maintain
        }
      },
      SamplingTrigger::ResourceUsage => {
        // 高资源使用时减少采样率
        if metrics.cpu_usage_percent > 80.0 || metrics.memory_usage_percent > 80.0 {
          SamplingDirection::Decrease
        } else if metrics.cpu_usage_percent < 50.0 && metrics.memory_usage_percent < 50.0 {
          SamplingDirection::Increase
        } else {
          SamplingDirection::Maintain
        }
      },
      SamplingTrigger::TimeWindow => {
        // 时间窗口触发可以根据历史趋势调整
        SamplingDirection::Maintain
      },
      SamplingTrigger::ManualOverride => {
        // 手动覆盖方向由外部指定
        SamplingDirection::Maintain
      }
    }
  }
  
  // 调整采样率
  let adjust_sampling_rate = fn(current_state: SamplingState, direction: SamplingDirection, strategy: SamplingAdjustmentStrategy) -> Float {
    let new_rate = match direction {
      SamplingDirection::Increase => {
        let proposed_rate = current_state.current_rate * strategy.adjustment_factor
        if proposed_rate > strategy.max_sampling_rate {
          strategy.max_sampling_rate
        } else {
          proposed_rate
        }
      },
      SamplingDirection::Decrease => {
        let proposed_rate = current_state.current_rate / strategy.adjustment_factor
        if proposed_rate < strategy.min_sampling_rate {
          strategy.min_sampling_rate
        } else {
          proposed_rate
        }
      },
      SamplingDirection::Maintain => current_state.current_rate
    }
    
    new_rate
  }
  
  // 应用采样策略
  let apply_sampling_strategy = fn(state: SamplingState, strategies: Array[SamplingAdjustmentStrategy], metrics: SystemMetrics, current_time: Int) -> SamplingState {
    let mut new_state = state
    let mut should_adjust = false
    let mut adjustment_direction = SamplingDirection::Maintain
    let mut active_strategy = strategies[0] // 默认使用第一个策略
    
    // 检查每个策略的触发条件
    for strategy in strategies {
      // 检查冷却期
      if current_time - state.last_adjustment_time < strategy.cooldown_period_ms {
        continue
      }
      
      // 检查触发条件
      if check_trigger_conditions(strategy.trigger, strategy.trigger_conditions, metrics) {
        should_adjust = true
        adjustment_direction = calculate_adjustment_direction(strategy.trigger, metrics, state.current_rate)
        active_strategy = strategy
        break
      }
    }
    
    // 如果需要调整，则调整采样率
    if should_adjust {
      let new_rate = adjust_sampling_rate(state, adjustment_direction, active_strategy)
      
      new_state = {
        current_rate: new_rate,
        last_adjustment_time: current_time,
        adjustment_count: state.adjustment_count + 1,
        total_samples_collected: state.total_samples_collected,
        total_events_processed: state.total_events_processed,
        effective_sampling_rate: (state.total_samples_collected as Float) / (state.total_events_processed as Float)
      }
    }
    
    new_state
  }
  
  // 创建测试策略
  let throughput_strategy = {
    name: "throughput_based",
    trigger: SamplingTrigger::ThroughputThreshold,
    trigger_conditions: Map::from([("max_throughput", "1000.0")]),
    adjustment_factor: 1.5,
    min_sampling_rate: 0.01,
    max_sampling_rate: 1.0,
    cooldown_period_ms: 60000 // 1分钟冷却期
  }
  
  let error_rate_strategy = {
    name: "error_rate_based",
    trigger: SamplingTrigger::ErrorRateThreshold,
    trigger_conditions: Map::from([("error_rate_threshold", "5.0")]),
    adjustment_factor: 2.0,
    min_sampling_rate: 0.01,
    max_sampling_rate: 1.0,
    cooldown_period_ms: 30000 // 30秒冷却期
  }
  
  let resource_usage_strategy = {
    name: "resource_usage_based",
    trigger: SamplingTrigger::ResourceUsage,
    trigger_conditions: Map::from([("cpu_threshold", "80.0"), ("memory_threshold", "80.0")]),
    adjustment_factor: 1.2,
    min_sampling_rate: 0.01,
    max_sampling_rate: 0.8,
    cooldown_period_ms: 120000 // 2分钟冷却期
  }
  
  let strategies = [throughput_strategy, error_rate_strategy, resource_usage_strategy]
  
  // 初始采样状态
  let initial_state = {
    current_rate: 0.1,
    last_adjustment_time: 0,
    adjustment_count: 0,
    total_samples_collected: 1000,
    total_events_processed: 10000,
    effective_sampling_rate: 0.1
  }
  
  // 测试场景1: 高吞吐量触发调整
  let high_throughput_metrics = {
    throughput_events_per_sec: 1500.0,
    error_rate_percent: 1.0,
    cpu_usage_percent: 60.0,
    memory_usage_percent: 50.0,
    disk_io_mb_per_sec: 50.0,
    network_io_mb_per_sec: 100.0
  }
  
  let state_after_throughput = apply_sampling_strategy(initial_state, strategies, high_throughput_metrics, 61000)
  
  // 验证吞吐量策略触发
  assert_true(state_after_throughput.current_rate < initial_state.current_rate) // 应该减少采样率
  assert_eq(state_after_throughput.adjustment_count, 1)
  assert_eq(state_after_throughput.last_adjustment_time, 61000)
  
  // 测试场景2: 高错误率触发调整
  let high_error_rate_metrics = {
    throughput_events_per_sec: 500.0,
    error_rate_percent: 8.0,
    cpu_usage_percent: 60.0,
    memory_usage_percent: 50.0,
    disk_io_mb_per_sec: 50.0,
    network_io_mb_per_sec: 100.0
  }
  
  let state_after_error = apply_sampling_strategy(initial_state, strategies, high_error_rate_metrics, 31000)
  
  // 验证错误率策略触发
  assert_true(state_after_error.current_rate > initial_state.current_rate) // 应该增加采样率
  assert_eq(state_after_error.adjustment_count, 1)
  assert_eq(state_after_error.last_adjustment_time, 31000)
  
  // 测试场景3: 高资源使用触发调整
  let high_resource_metrics = {
    throughput_events_per_sec: 500.0,
    error_rate_percent: 1.0,
    cpu_usage_percent: 85.0,
    memory_usage_percent: 50.0,
    disk_io_mb_per_sec: 50.0,
    network_io_mb_per_sec: 100.0
  }
  
  let state_after_resource = apply_sampling_strategy(initial_state, strategies, high_resource_metrics, 121000)
  
  // 验证资源使用策略触发
  assert_true(state_after_resource.current_rate < initial_state.current_rate) // 应该减少采样率
  assert_eq(state_after_resource.adjustment_count, 1)
  assert_eq(state_after_resource.last_adjustment_time, 121000)
  
  // 测试场景4: 冷却期内的调整请求被忽略
  let state_within_cooldown = apply_sampling_strategy(state_after_throughput, strategies, high_throughput_metrics, 65000)
  
  // 验证冷却期内不调整
  assert_eq(state_within_cooldown.current_rate, state_after_throughput.current_rate)
  assert_eq(state_within_cooldown.adjustment_count, state_after_throughput.adjustment_count)
  assert_eq(state_within_cooldown.last_adjustment_time, state_after_throughput.last_adjustment_time)
  
  // 测试场景5: 采样率边界限制
  let very_low_rate_state = { initial_state | current_rate: 0.01, min_sampling_rate: 0.01 }
  let very_high_rate_state = { initial_state | current_rate: 1.0, max_sampling_rate: 1.0 }
  
  // 尝试进一步减少最低采样率
  let state_after_min_limit = apply_sampling_strategy(very_low_rate_state, strategies, high_throughput_metrics, 121000)
  assert_eq(state_after_min_limit.current_rate, 0.01) // 不应该低于最小采样率
  
  // 尝试进一步提高最高采样率
  let state_after_max_limit = apply_sampling_strategy(very_high_rate_state, strategies, high_error_rate_metrics, 31000)
  assert_eq(state_after_max_limit.current_rate, 1.0) // 不应该高于最大采样率
}

// 测试2: 智能采样决策算法
test "智能采样决策算法功能" {
  // 定义事件重要性评分因素
  enum ImportanceFactor {
    ErrorStatus      // 错误状态
    HighDuration     // 高持续时间
    CriticalService  // 关键服务
    SensitiveUser    // 敏感用户
    BusinessImpact   // 业务影响
    NewOperation     // 新操作
  }
  
  // 定义采样决策
  type SamplingDecision = {
    should_sample: Bool
    confidence: Float
    reason: String
    priority_score: Float
  }
  
  // 定义事件特征
  type EventFeatures = {
    trace_id: String
    span_id: String
    service_name: String
    operation_name: String
    status: String
    duration_ms: Int
    tags: Map[String, String]
    timestamp: Int
  }
  
  // 定义评分规则
  type ScoringRule = {
    factor: ImportanceFactor
    weight: Float
    condition: (EventFeatures) -> Bool
    score: Float
  }
  
  // 定义采样决策模型
  type SamplingDecisionModel = {
    name: String
    scoring_rules: Array[ScoringRule]
    sampling_threshold: Float
    adaptive_threshold: Bool
  }
  
  // 计算事件重要性评分
  let calculate_importance_score = fn(event: EventFeatures, model: SamplingDecisionModel) -> Float {
    let mut total_score = 0.0
    
    for rule in model.scoring_rules {
      if rule.condition(event) {
        total_score = total_score + (rule.score * rule.weight)
      }
    }
    
    total_score
  }
  
  // 做出采样决策
  let make_sampling_decision = fn(event: EventFeatures, model: SamplingDecisionModel, current_sampling_rate: Float) -> SamplingDecision {
    let importance_score = calculate_importance_score(event, model)
    
    // 基础采样概率
    let base_probability = current_sampling_rate
    
    // 根据重要性评分调整采样概率
    let adjusted_probability = if importance_score > 0.0 {
      (base_probability + (importance_score / 100.0)).min(1.0)
    } else {
      base_probability
    }
    
    // 应用阈值
    let threshold = if model.adaptive_threshold {
      // 自适应阈值：基于事件重要性动态调整
      (model.sampling_threshold + (importance_score / 200.0)).min(1.0)
    } else {
      model.sampling_threshold
    }
    
    // 生成随机数模拟采样决策
    let random_value = 0.5 // 简化为固定值，实际应该是随机数
    
    let should_sample = adjusted_probability >= threshold
    
    // 确定决策原因
    let reason = if importance_score > 50.0 {
      "高重要性事件，强制采样"
    } else if importance_score > 20.0 {
      "中等重要性事件，提高采样概率"
    } else if should_sample {
      "常规采样"
    } else {
      "未达到采样阈值"
    }
    
    let confidence = if importance_score > 50.0 {
      0.9
    } else if importance_score > 20.0 {
      0.7
    } else {
      0.5
    }
    
    {
      should_sample,
      confidence,
      reason,
      priority_score: importance_score
    }
  }
  
  // 创建评分规则
  let scoring_rules = [
    {
      factor: ImportanceFactor::ErrorStatus,
      weight: 30.0,
      condition: fn(event: EventFeatures) { event.status == "error" || event.status == "timeout" },
      score: 50.0
    },
    {
      factor: ImportanceFactor::HighDuration,
      weight: 20.0,
      condition: fn(event: EventFeatures) { event.duration_ms > 1000 },
      score: 30.0
    },
    {
      factor: ImportanceFactor::CriticalService,
      weight: 25.0,
      condition: fn(event: EventFeatures) { 
        event.service_name == "payment-service" || 
        event.service_name == "auth-service" ||
        event.service_name == "order-service"
      },
      score: 25.0
    },
    {
      factor: ImportanceFactor::SensitiveUser,
      weight: 15.0,
      condition: fn(event: EventFeatures) { 
        match Map::get(event.tags, "user.tier") {
          Some(tier) => tier == "premium" || tier == "enterprise",
          None => false
        }
      },
      score: 20.0
    },
    {
      factor: ImportanceFactor::BusinessImpact,
      weight: 20.0,
      condition: fn(event: EventFeatures) { 
        match Map::get(event.tags, "business.critical") {
          Some(value) => value == "true",
          None => false
        }
      },
      score: 40.0
    },
    {
      factor: ImportanceFactor::NewOperation,
      weight: 10.0,
      condition: fn(event: EventFeatures) { 
        match Map::get(event.tags, "operation.new") {
          Some(value) => value == "true",
          None => false
        }
      },
      score: 15.0
    }
  ]
  
  // 创建采样决策模型
  let standard_model = {
    name: "standard_sampling_model",
    scoring_rules,
    sampling_threshold: 0.3,
    adaptive_threshold: false
  }
  
  let adaptive_model = {
    name: "adaptive_sampling_model",
    scoring_rules,
    sampling_threshold: 0.3,
    adaptive_threshold: true
  }
  
  // 创建测试事件
  let error_event = {
    trace_id: "trace-001",
    span_id: "span-001",
    service_name: "payment-service",
    operation_name: "process_payment",
    status: "error",
    duration_ms: 1500,
    tags: Map::from([("user.tier", "premium"), ("business.critical", "true")]),
    timestamp: 1640995200000
  }
  
  let normal_event = {
    trace_id: "trace-002",
    span_id: "span-002",
    service_name: "logging-service",
    operation_name: "write_log",
    status: "ok",
    duration_ms: 50,
    tags: Map::from([("user.tier", "standard")]),
    timestamp: 1640995200000
  }
  
  let critical_event = {
    trace_id: "trace-003",
    span_id: "span-003",
    service_name: "auth-service",
    operation_name: "authenticate",
    status: "timeout",
    duration_ms: 2000,
    tags: Map::from([("user.tier", "enterprise"), ("business.critical", "true"), ("operation.new", "true")]),
    timestamp: 1640995200000
  }
  
  // 测试标准模型决策
  let error_decision = make_sampling_decision(error_event, standard_model, 0.1)
  let normal_decision = make_sampling_decision(normal_event, standard_model, 0.1)
  let critical_decision = make_sampling_decision(critical_event, standard_model, 0.1)
  
  // 验证错误事件决策
  assert_true(error_decision.should_sample) // 错误事件应该被采样
  assert_true(error_decision.priority_score > 50.0) // 高优先级分数
  assert_eq(error_decision.reason, "高重要性事件，强制采样")
  assert_eq(error_decision.confidence, 0.9)
  
  // 验证普通事件决策
  assert_false(normal_decision.should_sample) // 普通事件可能不被采样（低采样率）
  assert_true(normal_decision.priority_score < 20.0) // 低优先级分数
  assert_eq(normal_decision.reason, "未达到采样阈值")
  
  // 验证关键事件决策
  assert_true(critical_decision.should_sample) // 关键事件应该被采样
  assert_true(critical_decision.priority_score > error_decision.priority_score) // 比错误事件优先级更高
  assert_eq(critical_decision.reason, "高重要性事件，强制采样")
  
  // 测试自适应模型决策
  let adaptive_error_decision = make_sampling_decision(error_event, adaptive_model, 0.1)
  let adaptive_normal_decision = make_sampling_decision(normal_event, adaptive_model, 0.1)
  
  // 验证自适应模型对高重要性事件更敏感
  assert_true(adaptive_error_decision.should_sample)
  assert_true(adaptive_error_decision.priority_score == error_decision.priority_score) // 评分应该相同
  
  // 测试不同采样率下的决策
  let high_rate_error_decision = make_sampling_decision(error_event, standard_model, 0.5)
  let high_rate_normal_decision = make_sampling_decision(normal_event, standard_model, 0.5)
  
  // 验证高采样率下普通事件也可能被采样
  assert_true(high_rate_error_decision.should_sample)
  assert_true(high_rate_normal_decision.should_sample) // 高采样率下普通事件也被采样
  assert_eq(high_rate_normal_decision.reason, "常规采样")
}

// 测试3: 采样效果评估与优化
test "采样效果评估与优化功能" {
  // 定义采样效果指标
  type SamplingEffectivenessMetrics = {
    accuracy_score: Float          // 准确性评分
    coverage_score: Float          // 覆盖率评分
    representativeness_score: Float // 代表性评分
    resource_efficiency_score: Float // 资源效率评分
    overall_score: Float           // 总体评分
  }
  
  // 定义采样配置
  type SamplingConfiguration = {
    name: String
    base_sampling_rate: Float
    adaptive_rules: Array[String]
    target_metrics: Array[String]
    optimization_objective: String
  }
  
  // 定义采样结果
  type SamplingResult = {
    configuration: SamplingConfiguration
    total_events: Int
    sampled_events: Int
    metrics: SamplingEffectivenessMetrics
    cost_estimate: Float
  }
  
  // 定义优化建议
  type OptimizationRecommendation = {
    configuration_name: String
    recommended_changes: Array[String]
    expected_improvement: Float
    implementation_effort: String
  }
  
  // 计算准确性评分
  let calculate_accuracy_score = fn(sampled_data: Array[Map[String, String]], full_data: Array[Map[String, String]]) -> Float {
    if sampled_data.length() == 0 || full_data.length() == 0 {
      return 0.0
    }
    
    // 简化的准确性计算：比较关键指标的分布
    let key_metrics = ["error_rate", "avg_duration", "status_distribution"]
    let mut accuracy_scores = []
    
    for metric in key_metrics {
      // 计算采样数据和完整数据的指标差异
      let sampled_value = match metric {
        "error_rate" => {
          let errors = sampled_data.filter(fn(d) => {
            match Map::get(d, "status") {
              Some(status) => status == "error",
              None => false
            }
          }).length()
          
          (errors as Float) / (sampled_data.length() as Float) * 100.0
        },
        "avg_duration" => {
          let durations = sampled_data.map(fn(d) => {
            match Map::get(d, "duration") {
              Some(duration) => duration.to_float(),
              None => 0.0
            }
          })
          
          if durations.length() > 0 {
            durations.reduce(fn(acc, d) { acc + d }, 0.0) / (durations.length() as Float)
          } else {
            0.0
          }
        },
        "status_distribution" => {
          // 简化：返回状态类型的数量
          let statuses = sampled_data.map(fn(d) => {
            match Map::get(d, "status") {
              Some(status) => status,
              None => "unknown"
            }
          })
          
          let unique_statuses = statuses.reduce(fn(acc, status) => {
            if acc.contains(status) { acc } else { acc.push(status) }
          }, [])
          
          unique_statuses.length() as Float
        },
        _ => 0.0
      }
      
      let full_value = match metric {
        "error_rate" => {
          let errors = full_data.filter(fn(d) => {
            match Map::get(d, "status") {
              Some(status) => status == "error",
              None => false
            }
          }).length()
          
          (errors as Float) / (full_data.length() as Float) * 100.0
        },
        "avg_duration" => {
          let durations = full_data.map(fn(d) => {
            match Map::get(d, "duration") => {
              Some(duration) => duration.to_float(),
              None => 0.0
            }
          })
          
          if durations.length() > 0 {
            durations.reduce(fn(acc, d) { acc + d }, 0.0) / (durations.length() as Float)
          } else {
            0.0
          }
        },
        "status_distribution" => {
          let statuses = full_data.map(fn(d) => {
            match Map::get(d, "status") {
              Some(status) => status,
              None => "unknown"
            }
          })
          
          let unique_statuses = statuses.reduce(fn(acc, status) => {
            if acc.contains(status) { acc } else { acc.push(status) }
          }, [])
          
          unique_statuses.length() as Float
        },
        _ => 0.0
      }
      
      // 计算差异百分比
      let difference_percentage = if full_value != 0.0 {
        ((sampled_value - full_value).abs() / full_value) * 100.0
      } else {
        if sampled_value == 0.0 { 0.0 } else { 100.0 }
      }
      
      // 转换为准确性评分（差异越小，准确性越高）
      let accuracy = if difference_percentage > 100.0 {
        0.0
      } else {
        100.0 - difference_percentage
      }
      
      accuracy_scores = accuracy_scores.push(accuracy)
    }
    
    // 计算平均准确性
    if accuracy_scores.length() > 0 {
      accuracy_scores.reduce(fn(acc, score) { acc + score }, 0.0) / (accuracy_scores.length() as Float)
    } else {
      0.0
    }
  }
  
  // 计算覆盖率评分
  let calculate_coverage_score = fn(sampled_data: Array[Map[String, String]], full_data: Array[Map[String, String]]) -> Float {
    if sampled_data.length() == 0 || full_data.length() == 0 {
      return 0.0
    }
    
    // 计算采样比例
    let sampling_ratio = (sampled_data.length() as Float) / (full_data.length() as Float)
    
    // 计算关键路径覆盖率
    let critical_traces = full_data.filter(fn(d) => {
      match Map::get(d, "critical") {
        Some(value) => value == "true",
        None => false
      }
    })
    
    let sampled_critical_traces = sampled_data.filter(fn(d) => {
      match Map::get(d, "critical") {
        Some(value) => value == "true",
        None => false
      }
    })
    
    let critical_coverage = if critical_traces.length() > 0 {
      (sampled_critical_traces.length() as Float) / (critical_traces.length() as Float)
    } else {
      1.0
    }
    
    // 综合覆盖率评分
    (sampling_ratio * 50.0) + (critical_coverage * 50.0)
  }
  
  // 计算代表性评分
  let calculate_representativeness_score = fn(sampled_data: Array[Map[String, String]], full_data: Array[Map[String, String]]) -> Float {
    if sampled_data.length() == 0 || full_data.length() == 0 {
      return 0.0
    }
    
    // 简化的代表性计算：检查服务分布的相似性
    let full_services = full_data.map(fn(d) => {
      match Map::get(d, "service") {
        Some(service) => service,
        None => "unknown"
      }
    })
    
    let sampled_services = sampled_data.map(fn(d) => {
      match Map::get(d, "service") {
        Some(service) => service,
        None => "unknown"
      }
    })
    
    // 计算每个服务在完整数据和采样数据中的比例
    let mut full_service_counts = Map::empty()
    let mut sampled_service_counts = Map::empty()
    
    for service in full_services {
      let current_count = match Map::get(full_service_counts, service) {
        Some(count) => count,
        None => 0
      }
      full_service_counts = Map::insert(full_service_counts, service, current_count + 1)
    }
    
    for service in sampled_services {
      let current_count = match Map::get(sampled_service_counts, service) {
        Some(count) => count,
        None => 0
      }
      sampled_service_counts = Map::insert(sampled_service_counts, service, current_count + 1)
    }
    
    // 计算分布差异
    let mut total_diff = 0.0
    let total_full = full_data.length() as Float
    let total_sampled = sampled_data.length() as Float
    
    for (service, full_count) in full_service_counts.to_array() {
      let full_ratio = (full_count as Float) / total_full
      let sampled_ratio = match Map::get(sampled_service_counts, service) {
        Some(count) => (count as Float) / total_sampled,
        None => 0.0
      }
      
      total_diff = total_diff + (full_ratio - sampled_ratio).abs()
    }
    
    // 转换为代表性评分（差异越小，代表性越高）
    let representativeness = if total_diff > 2.0 {
      0.0
    } else {
      100.0 * (1.0 - (total_diff / 2.0))
    }
    
    representativeness
  }
  
  // 计算资源效率评分
  let calculate_resource_efficiency_score = fn(sampled_data: Array[Map[String, String]], full_data: Array[Map[String, String]], cost_per_sample: Float) -> Float {
    if sampled_data.length() == 0 || full_data.length() == 0 {
      return 0.0
    }
    
    // 计算成本节约比例
    let sampling_ratio = (sampled_data.length() as Float) / (full_data.length() as Float)
    let cost_savings = 1.0 - sampling_ratio
    
    // 计算信息保留比例（简化）
    let information_retention = calculate_accuracy_score(sampled_data, full_data) / 100.0
    
    // 资源效率 = 成本节约 * 信息保留
    (cost_savings * information_retention) * 100.0
  }
  
  // 评估采样配置
  let evaluate_sampling_configuration = fn(config: SamplingConfiguration, full_data: Array[Map[String, String]], cost_per_sample: Float) -> SamplingResult {
    // 简化的采样过程：根据基础采样率采样
    let sampling_rate = config.base_sampling_rate
    let sample_count = ((full_data.length() as Float) * sampling_rate) as Int
    
    let sampled_data = full_data.slice(0, sample_count)
    
    // 计算各项指标
    let accuracy_score = calculate_accuracy_score(sampled_data, full_data)
    let coverage_score = calculate_coverage_score(sampled_data, full_data)
    let representativeness_score = calculate_representativeness_score(sampled_data, full_data)
    let resource_efficiency_score = calculate_resource_efficiency_score(sampled_data, full_data, cost_per_sample)
    
    // 计算总体评分
    let overall_score = (accuracy_score + coverage_score + representativeness_score + resource_efficiency_score) / 4.0
    
    let metrics = {
      accuracy_score,
      coverage_score,
      representativeness_score,
      resource_efficiency_score,
      overall_score
    }
    
    // 估算成本
    let cost_estimate = (sampled_data.length() as Float) * cost_per_sample
    
    {
      configuration: config,
      total_events: full_data.length(),
      sampled_events: sampled_data.length(),
      metrics,
      cost_estimate
    }
  }
  
  // 生成优化建议
  let generate_optimization_recommendations = fn(results: Array[SamplingResult]) -> Array[OptimizationRecommendation] {
    let mut recommendations = []
    
    for result in results {
      let mut changes = []
      let mut expected_improvement = 0.0
      
      // 基于指标生成建议
      if result.metrics.accuracy_score < 70.0 {
        changes = changes.push("增加错误事件的采样权重")
        changes = changes.push("提高关键服务的采样率")
        expected_improvement = expected_improvement + 15.0
      }
      
      if result.metrics.coverage_score < 60.0 {
        changes = changes.push("增加整体采样率")
        changes = changes.push("确保关键路径的完整覆盖")
        expected_improvement = expected_improvement + 20.0
      }
      
      if result.metrics.representativeness_score < 65.0 {
        changes = changes.push("调整采样策略以确保服务分布的代表性")
        changes = changes.push("实施分层采样策略")
        expected_improvement = expected_improvement + 10.0
      }
      
      if result.metrics.resource_efficiency_score < 50.0 {
        changes = changes.push("优化采样算法以减少资源使用")
        changes = changes.push("实施更智能的采样决策")
        expected_improvement = expected_improvement + 25.0
      }
      
      if changes.length() > 0 {
        let implementation_effort = if changes.length() <= 2 {
          "低"
        } else if changes.length() <= 4 {
          "中"
        } else {
          "高"
        }
        
        let recommendation = {
          configuration_name: result.configuration.name,
          recommended_changes: changes,
          expected_improvement,
          implementation_effort
        }
        
        recommendations = recommendations.push(recommendation)
      }
    }
    
    recommendations
  }
  
  // 创建测试数据
  let full_data = [
    Map::from([("service", "auth-service"), ("status", "ok"), ("duration", "100"), ("critical", "true")]),
    Map::from([("service", "auth-service"), ("status", "ok"), ("duration", "120"), ("critical", "true")]),
    Map::from([("service", "payment-service"), ("status", "error"), ("duration", "500"), ("critical", "true")]),
    Map::from([("service", "payment-service"), ("status", "ok"), ("duration", "200"), ("critical", "true")]),
    Map::from([("service", "user-service"), ("status", "ok"), ("duration", "80"), ("critical", "false")]),
    Map::from([("service", "user-service"), ("status", "ok"), ("duration", "90"), ("critical", "false")]),
    Map::from([("service", "logging-service"), ("status", "ok"), ("duration", "50"), ("critical", "false")]),
    Map::from([("service", "logging-service"), ("status", "ok"), ("duration", "60"), ("critical", "false")]),
    Map::from([("service", "notification-service"), ("status", "timeout"), ("duration", "1000"), ("critical", "false")]),
    Map::from([("service", "notification-service"), ("status", "ok"), ("duration", "70"), ("critical", "false")])
  ]
  
  // 创建采样配置
  let low_rate_config = {
    name: "low_rate_sampling",
    base_sampling_rate: 0.1,
    adaptive_rules: ["error_boost", "critical_service_boost"],
    target_metrics: ["error_rate", "avg_duration"],
    optimization_objective: "resource_efficiency"
  }
  
  let medium_rate_config = {
    name: "medium_rate_sampling",
    base_sampling_rate: 0.3,
    adaptive_rules: ["error_boost", "critical_service_boost", "high_duration_boost"],
    target_metrics: ["error_rate", "avg_duration", "service_distribution"],
    optimization_objective: "balanced"
  }
  
  let high_rate_config = {
    name: "high_rate_sampling",
    base_sampling_rate: 0.6,
    adaptive_rules: ["error_boost", "critical_service_boost", "high_duration_boost", "new_operation_boost"],
    target_metrics: ["error_rate", "avg_duration", "service_distribution", "coverage"],
    optimization_objective: "accuracy"
  }
  
  // 评估采样配置
  let cost_per_sample = 0.01 // 每个样本0.01单位成本
  
  let low_rate_result = evaluate_sampling_configuration(low_rate_config, full_data, cost_per_sample)
  let medium_rate_result = evaluate_sampling_configuration(medium_rate_config, full_data, cost_per_sample)
  let high_rate_result = evaluate_sampling_configuration(high_rate_config, full_data, cost_per_sample)
  
  let results = [low_rate_result, medium_rate_result, high_rate_result]
  
  // 验证评估结果
  for result in results {
    assert_eq(result.total_events, full_data.length())
    assert_true(result.sampled_events > 0)
    assert_true(result.sampled_events <= result.total_events)
    assert_true(result.metrics.overall_score >= 0.0 && result.metrics.overall_score <= 100.0)
    assert_true(result.cost_estimate > 0.0)
  }
  
  // 验证不同采样率的影响
  assert_true(low_rate_result.sampled_events < medium_rate_result.sampled_events)
  assert_true(medium_rate_result.sampled_events < high_rate_result.sampled_events)
  
  assert_true(low_rate_result.cost_estimate < medium_rate_result.cost_estimate)
  assert_true(medium_rate_result.cost_estimate < high_rate_result.cost_estimate)
  
  // 高采样率应该有更高的准确性
  assert_true(high_rate_result.metrics.accuracy_score >= medium_rate_result.metrics.accuracy_score)
  assert_true(medium_rate_result.metrics.accuracy_score >= low_rate_result.metrics.accuracy_score)
  
  // 高采样率应该有更高的覆盖率
  assert_true(high_rate_result.metrics.coverage_score >= medium_rate_result.metrics.coverage_score)
  assert_true(medium_rate_result.metrics.coverage_score >= low_rate_result.metrics.coverage_score)
  
  // 低采样率应该有更高的资源效率
  assert_true(low_rate_result.metrics.resource_efficiency_score >= medium_rate_result.metrics.resource_efficiency_score)
  
  // 生成优化建议
  let recommendations = generate_optimization_recommendations(results)
  
  // 验证优化建议
  assert_true(recommendations.length() > 0)
  
  for recommendation in recommendations {
    assert_true(recommendation.recommended_changes.length() > 0)
    assert_true(recommendation.expected_improvement > 0.0)
    assert_true(
      recommendation.implementation_effort == "低" ||
      recommendation.implementation_effort == "中" ||
      recommendation.implementation_effort == "高"
    )
  }
  
  // 低采样率配置应该有提高采样率的建议
  let low_rate_recommendation = recommendations.find(fn(r) { r.configuration_name == "low_rate_sampling" })
  match low_rate_recommendation {
    Some(rec) => {
      let has_increase_rate_suggestion = rec.recommended_changes.any(fn(change) {
        change.contains("增加整体采样率") || change.contains("提高关键服务的采样率")
      })
      assert_true(has_increase_rate_suggestion)
    },
    None => assert_true(false)
  }
}