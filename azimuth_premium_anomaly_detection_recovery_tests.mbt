// Azimuth 异常检测和恢复测试用例
// 专注于遥测系统中的异常检测、告警和自动恢复机制

// 测试1: 性能异常检测算法
test "性能异常检测算法" {
  // 性能指标数据点
  type MetricPoint = {
    timestamp: Int,
    value: Float,
    metadata: Map[String, String]
  }
  
  // 异常检测结果
  type AnomalyResult = {
    is_anomaly: Bool,
    confidence: Float,
    threshold: Float,
    deviation: Float,
    anomaly_type: String
  }
  
  // 统计异常检测器（基于Z-Score）
  let statistical_anomaly_detector = fn(points: Array[MetricPoint], threshold: Float) {
    if points.length() < 3 {
      return {
        is_anomaly: false,
        confidence: 0.0,
        threshold: threshold,
        deviation: 0.0,
        anomaly_type: "insufficient_data"
      }
    }
    
    // 计算均值和标准差
    let values = points.map(fn(p) { p.value })
    let mean = values.reduce(fn(acc, v) { acc + v }, 0.0) / (values.length() as Float)
    
    let variance = values.reduce(fn(acc, v) { acc + (v - mean) * (v - mean) }, 0.0) / (values.length() as Float)
    let std_dev = variance.sqrt()
    
    if std_dev == 0.0 {
      return {
        is_anomaly: false,
        confidence: 0.0,
        threshold: threshold,
        deviation: 0.0,
        anomaly_type: "no_variance"
      }
    }
    
    // 检查最后一个点是否异常
    let last_point = points[points.length() - 1]
    let z_score = (last_point.value - mean).abs() / std_dev
    let is_anomaly = z_score > threshold
    let confidence = (z_score / threshold).min(1.0)
    
    {
      is_anomaly: is_anomaly,
      confidence: confidence,
      threshold: threshold,
      deviation: z_score,
      anomaly_type: if is_anomaly { "statistical_outlier" } else { "normal" }
    }
  }
  
  // 移动平均异常检测器
  let moving_average_anomaly_detector = fn(points: Array[MetricPoint], window_size: Int, threshold: Float) {
    if points.length() < window_size + 1 {
      return {
        is_anomaly: false,
        confidence: 0.0,
        threshold: threshold,
        deviation: 0.0,
        anomaly_type: "insufficient_data"
      }
    }
    
    // 计算移动平均
    let recent_points = points.slice(points.length() - window_size - 1, points.length() - 1)
    let avg = recent_points.reduce(fn(acc, p) { acc + p.value }, 0.0) / (recent_points.length() as Float)
    
    let last_point = points[points.length() - 1]
    let deviation = (last_point.value - avg).abs()
    let is_anomaly = deviation > threshold
    let confidence = (deviation / threshold).min(1.0)
    
    {
      is_anomaly: is_anomaly,
      confidence: confidence,
      threshold: threshold,
      deviation: deviation,
      anomaly_type: if is_anomaly { "moving_average_outlier" } else { "normal" }
    }
  }
  
  // 创建测试数据
  let normal_metrics = [
    { timestamp: 1000, value: 10.0, metadata: [ ("service", "api") ] },
    { timestamp: 2000, value: 12.0, metadata: [ ("service", "api") ] },
    { timestamp: 3000, value: 11.0, metadata: [ ("service", "api") ] },
    { timestamp: 4000, value: 13.0, metadata: [ ("service", "api") ] },
    { timestamp: 5000, value: 9.0, metadata: [ ("service", "api") ] }
  ]
  
  let anomaly_metrics = normal_metrics + [
    { timestamp: 6000, value: 50.0, metadata: [ ("service", "api") ] } // 异常值
  ]
  
  // 测试统计异常检测
  let normal_result = statistical_anomaly_detector(normal_metrics, 2.0)
  assert_false(normal_result.is_anomaly)
  assert_eq(normal_result.anomaly_type, "normal")
  
  let anomaly_result = statistical_anomaly_detector(anomaly_metrics, 2.0)
  assert_true(anomaly_result.is_anomaly)
  assert_eq(anomaly_result.anomaly_type, "statistical_outlier")
  assert_true(anomaly_result.confidence > 0.0)
  
  // 测试移动平均异常检测
  let ma_normal_result = moving_average_anomaly_detector(normal_metrics, 3, 10.0)
  assert_false(ma_normal_result.is_anomaly)
  assert_eq(ma_normal_result.anomaly_type, "normal")
  
  let ma_anomaly_result = moving_average_anomaly_detector(anomaly_metrics, 3, 10.0)
  assert_true(ma_anomaly_result.is_anomaly)
  assert_eq(ma_anomaly_result.anomaly_type, "moving_average_outlier")
  assert_true(ma_anomaly_result.confidence > 0.0)
  
  // 测试边界情况
  let empty_metrics: Array[MetricPoint] = []
  let empty_result = statistical_anomaly_detector(empty_metrics, 2.0)
  assert_false(empty_result.is_anomaly)
  assert_eq(empty_result.anomaly_type, "insufficient_data")
}

// 测试2: 错误率异常检测
test "错误率异常检测" {
  // 错误率数据点
  type ErrorRatePoint = {
    timestamp: Int,
    total_requests: Int,
    error_requests: Int,
    service_name: String,
    endpoint: String
  }
  
  // 错误率异常检测结果
  type ErrorRateAnomaly = {
    is_anomaly: Bool,
    current_error_rate: Float,
    baseline_error_rate: Float,
    threshold: Float,
    deviation: Float
  }
  
  // 错误率异常检测器
  let error_rate_anomaly_detector = fn(points: Array[ErrorRatePoint], baseline_window: Int, threshold: Float) {
    if points.length() < baseline_window + 1 {
      return {
        is_anomaly: false,
        current_error_rate: 0.0,
        baseline_error_rate: 0.0,
        threshold: threshold,
        deviation: 0.0
      }
    }
    
    // 计算基线错误率
    let baseline_points = points.slice(points.length() - baseline_window - 1, points.length() - 1)
    let baseline_total = baseline_points.reduce(fn(acc, p) { acc + p.total_requests }, 0)
    let baseline_errors = baseline_points.reduce(fn(acc, p) { acc + p.error_requests }, 0)
    let baseline_error_rate = if baseline_total > 0 {
      (baseline_errors as Float) / (baseline_total as Float)
    } else {
      0.0
    }
    
    // 计算当前错误率
    let current_point = points[points.length() - 1]
    let current_error_rate = if current_point.total_requests > 0 {
      (current_point.error_requests as Float) / (current_point.total_requests as Float)
    } else {
      0.0
    }
    
    // 检测异常
    let deviation = (current_error_rate - baseline_error_rate).abs()
    let is_anomaly = deviation > threshold
    
    {
      is_anomaly: is_anomaly,
      current_error_rate: current_error_rate,
      baseline_error_rate: baseline_error_rate,
      threshold: threshold,
      deviation: deviation
    }
  }
  
  // 创建测试数据
  let normal_error_rates = [
    { timestamp: 1000, total_requests: 1000, error_requests: 10, service_name: "api", endpoint: "/users" },
    { timestamp: 2000, total_requests: 1200, error_requests: 15, service_name: "api", endpoint: "/users" },
    { timestamp: 3000, total_requests: 900, error_requests: 12, service_name: "api", endpoint: "/users" },
    { timestamp: 4000, total_requests: 1100, error_requests: 8, service_name: "api", endpoint: "/users" },
    { timestamp: 5000, total_requests: 1300, error_requests: 14, service_name: "api", endpoint: "/users" }
  ]
  
  let spike_error_rates = normal_error_rates + [
    { timestamp: 6000, total_requests: 1000, error_requests: 200, service_name: "api", endpoint: "/users" } // 错误率激增
  ]
  
  // 测试错误率异常检测
  let normal_result = error_rate_anomaly_detector(normal_error_rates, 3, 0.05) // 5%阈值
  assert_false(normal_result.is_anomaly)
  assert_true(normal_result.current_error_rate < 0.05)
  
  let spike_result = error_rate_anomaly_detector(spike_error_rates, 3, 0.05) // 5%阈值
  assert_true(spike_result.is_anomaly)
  assert_true(spike_result.current_error_rate > 0.05)
  assert_true(spike_result.deviation > 0.05)
  
  // 测试零请求情况
  let zero_request_data = normal_error_rates + [
    { timestamp: 6000, total_requests: 0, error_requests: 0, service_name: "api", endpoint: "/users" }
  ]
  
  let zero_result = error_rate_anomaly_detector(zero_request_data, 3, 0.05)
  assert_eq(zero_result.current_error_rate, 0.0)
}

// 测试3: 自动恢复机制
test "自动恢复机制" {
  // 恢复动作类型
  enum RecoveryAction {
    RestartService
    ScaleUp
    ScaleDown
    RollbackDeployment
    EnableCircuitBreaker
    DisableCircuitBreaker
    ClearCache
    NoOp
  }
  
  // 恢复策略
  type RecoveryStrategy = {
    name: String,
    conditions: Array[String>,
    actions: Array[RecoveryAction],
    cooldown_period: Int,
    max_attempts: Int
  }
  
  // 恢复执行结果
  type RecoveryResult = {
    strategy_name: String,
    actions_executed: Array[RecoveryAction],
    success: Bool,
    message: String,
    remaining_attempts: Int
  }
  
  // 恢复管理器
  type RecoveryManager = {
    strategies: Array[RecoveryStrategy>,
    attempt_counts: Map[String, Int],
    last_execution: Map[String, Int]
  }
  
  let create_recovery_manager = fn(strategies: Array[RecoveryStrategy>) {
    {
      strategies: strategies,
      attempt_counts: Map::empty(),
      last_execution: Map::empty()
    }
  }
  
  let execute_recovery = fn(manager: RecoveryManager, strategy_name: String, current_time: Int) {
    // 查找策略
    let strategy_option = manager.strategies.find(fn(s) { s.name == strategy_name })
    
    match strategy_option {
      Some(strategy) => {
        // 检查冷却期
        let last_time = match Map::get(manager.last_execution, strategy_name) {
          Some(time) => time
          None => 0
        }
        
        if current_time - last_time < strategy.cooldown_period {
          return {
            strategy_name: strategy_name,
            actions_executed: [],
            success: false,
            message: "策略在冷却期内",
            remaining_attempts: match Map::get(manager.attempt_counts, strategy_name) {
              Some(count) => strategy.max_attempts - count
              None => strategy.max_attempts
            }
          }
        }
        
        // 检查尝试次数
        let current_attempts = match Map::get(manager.attempt_counts, strategy_name) {
          Some(count) => count
          None => 0
        }
        
        if current_attempts >= strategy.max_attempts {
          return {
            strategy_name: strategy_name,
            actions_executed: [],
            success: false,
            message: "已达到最大尝试次数",
            remaining_attempts: 0
          }
        }
        
        // 执行恢复动作
        let mut actions_executed = []
        let mut all_success = true
        
        for action in strategy.actions {
          // 模拟动作执行
          let action_success = match action {
            RecoveryAction::RestartService => true
            RecoveryAction::ScaleUp => true
            RecoveryAction::ScaleDown => true
            RecoveryAction::RollbackDeployment => true
            RecoveryAction::EnableCircuitBreaker => true
            RecoveryAction::DisableCircuitBreaker => true
            RecoveryAction::ClearCache => true
            RecoveryAction::NoOp => true
          }
          
          if action_success {
            actions_executed = actions_executed.push(action)
          } else {
            all_success = false
            break
          }
        }
        
        // 更新尝试次数和最后执行时间
        let _ = Map::insert(manager.attempt_counts, strategy_name, current_attempts + 1)
        let _ = Map::insert(manager.last_execution, strategy_name, current_time)
        
        {
          strategy_name: strategy_name,
          actions_executed: actions_executed,
          success: all_success && actions_executed.length() > 0,
          message: if all_success { "恢复动作执行成功" } else { "部分恢复动作失败" },
          remaining_attempts: strategy.max_attempts - (current_attempts + 1)
        }
      }
      None => {
        {
          strategy_name: strategy_name,
          actions_executed: [],
          success: false,
          message: "未找到指定的恢复策略",
          remaining_attempts: 0
        }
      }
    }
  }
  
  // 创建恢复策略
  let restart_strategy = {
    name: "service_restart",
    conditions: ["high_error_rate", "high_latency"],
    actions: [RecoveryAction::RestartService],
    cooldown_period: 300, // 5分钟
    max_attempts: 3
  }
  
  let scale_up_strategy = {
    name: "auto_scale_up",
    conditions: ["high_cpu_usage", "high_memory_usage"],
    actions: [RecoveryAction::ScaleUp],
    cooldown_period: 600, // 10分钟
    max_attempts: 5
  }
  
  let complex_strategy = {
    name: "complex_recovery",
    conditions: ["service_unavailable"],
    actions: [
      RecoveryAction::EnableCircuitBreaker,
      RecoveryAction::ClearCache,
      RecoveryAction::RestartService
    ],
    cooldown_period: 900, // 15分钟
    max_attempts: 2
  }
  
  // 创建恢复管理器
  let manager = create_recovery_manager([restart_strategy, scale_up_strategy, complex_strategy])
  
  // 测试恢复策略执行
  let result1 = execute_recovery(manager, "service_restart", 1000)
  assert_eq(result1.strategy_name, "service_restart")
  assert_true(result1.success)
  assert_eq(result1.actions_executed.length(), 1)
  assert_eq(result1.remaining_attempts, 2)
  
  // 测试冷却期
  let result2 = execute_recovery(manager, "service_restart", 1200) // 在冷却期内
  assert_false(result2.success)
  assert_true(result2.message.contains("冷却期"))
  
  // 测试冷却期后执行
  let result3 = execute_recovery(manager, "service_restart", 1500) // 超过冷却期
  assert_true(result3.success)
  assert_eq(result3.remaining_attempts, 1)
  
  // 测试复杂恢复策略
  let result4 = execute_recovery(manager, "complex_recovery", 2000)
  assert_true(result4.success)
  assert_eq(result4.actions_executed.length(), 3)
  assert_eq(result4.remaining_attempts, 1)
  
  // 测试不存在的策略
  let result5 = execute_recovery(manager, "nonexistent_strategy", 2500)
  assert_false(result5.success)
  assert_true(result5.message.contains("未找到"))
}

// 测试4: 告警规则引擎
test "告警规则引擎" {
  // 告警级别
  enum AlertSeverity {
    Info
    Warning
    Critical
    Emergency
  }
  
  // 告警条件
  type AlertCondition = {
    metric_name: String,
    operator: String, // >, <, >=, <=, ==, !=
    threshold: Float,
    duration: Int // 持续时间（秒）
  }
  
  // 告警规则
  type AlertRule = {
    name: String,
    description: String,
    severity: AlertSeverity,
    conditions: Array[AlertCondition>,
    labels: Map[String, String],
    annotations: Map[String, String],
    enabled: Bool
  }
  
  // 告警状态
  enum AlertState {
    Pending
    Firing
    Resolved
  }
  
  // 告警实例
  type Alert = {
    rule_name: String,
    state: AlertState,
    severity: AlertSeverity,
    value: Float,
    threshold: Float,
    start_time: Int,
    last_evaluation_time: Int,
    labels: Map[String, String],
    annotations: Map[String, String]
  }
  
  // 告警规则引擎
  type AlertRuleEngine = {
    rules: Array[AlertRule],
    active_alerts: Map[String, Alert]
  }
  
  let create_alert_engine = fn(rules: Array[AlertRule>) {
    {
      rules: rules,
      active_alerts: Map::empty()
    }
  }
  
  let evaluate_condition = fn(condition: AlertCondition, value: Float) {
    match condition.operator {
      ">" => value > condition.threshold
      "<" => value < condition.threshold
      ">=" => value >= condition.threshold
      "<=" => value <= condition.threshold
      "==" => value == condition.threshold
      "!=" => value != condition.threshold
      _ => false
    }
  }
  
  let evaluate_rules = fn(engine: AlertRuleEngine, metrics: Map[String, (Float, Int)>, current_time: Int) {
    let mut new_alerts = []
    
    for rule in engine.rules {
      if not rule.enabled {
        continue
      }
      
      let mut all_conditions_met = true
      let mut condition_values = []
      
      // 检查所有条件
      for condition in rule.conditions {
        match Map::get(metrics, condition.metric_name) {
          Some((value, timestamp)) => {
            condition_values = condition_values.push((condition.metric_name, value))
            
            // 检查时间窗口
            if current_time - timestamp > condition.duration {
              all_conditions_met = false
              break
            }
            
            // 检查条件
            if not evaluate_condition(condition, value) {
              all_conditions_met = false
              break
            }
          }
          None => {
            all_conditions_met = false
            break
          }
        }
      }
      
      // 处理告警状态
      match Map::get(engine.active_alerts, rule.name) {
        Some(existing_alert) => {
          if all_conditions_met {
            // 保持告警激活状态
            let updated_alert = { ...existing_alert, last_evaluation_time: current_time }
            let _ = Map::insert(engine.active_alerts, rule.name, updated_alert)
          } else {
            // 解析告警
            let resolved_alert = { ...existing_alert, state: AlertState::Resolved }
            let _ = Map::insert(engine.active_alerts, rule.name, resolved_alert)
          }
        }
        None => {
          if all_conditions_met {
            // 创建新告警
            let new_alert = {
              rule_name: rule.name,
              state: AlertState::Firing,
              severity: rule.severity,
              value: condition_values[0].1, // 使用第一个条件的值
              threshold: rule.conditions[0].threshold, // 使用第一个条件的阈值
              start_time: current_time,
              last_evaluation_time: current_time,
              labels: rule.labels,
              annotations: rule.annotations
            }
            
            let _ = Map::insert(engine.active_alerts, rule.name, new_alert)
            new_alerts = new_alerts.push(new_alert)
          }
        }
      }
    }
    
    new_alerts
  }
  
  // 创建告警规则
  let high_error_rate_rule = {
    name: "HighErrorRate",
    description: "服务错误率过高",
    severity: AlertSeverity::Critical,
    conditions: [
      {
        metric_name: "error_rate",
        operator: ">",
        threshold: 0.05,
        duration: 300
      }
    ],
    labels: [
      ("team", "platform"),
      ("service", "api")
    ],
    annotations: [
      ("summary", "API服务错误率超过5%"),
      ("description", "过去5分钟内API服务错误率持续超过5%，需要立即处理")
    ],
    enabled: true
  }
  
  let high_latency_rule = {
    name: "HighLatency",
    description: "服务延迟过高",
    severity: AlertSeverity::Warning,
    conditions: [
      {
        metric_name: "latency_p95",
        operator: ">",
        threshold: 1000.0,
        duration: 600
      }
    ],
    labels: [
      ("team", "platform"),
      ("service", "api")
    ],
    annotations: [
      ("summary", "API服务延迟过高"),
      ("description", "过去10分钟内API服务P95延迟超过1000ms")
    ],
    enabled: true
  }
  
  // 创建告警引擎
  let engine = create_alert_engine([high_error_rate_rule, high_latency_rule])
  
  // 测试告警触发
  let metrics1 = [
    ("error_rate", (0.1, 1000)), // 错误率10%，超过阈值
    ("latency_p95", (500.0, 1000)) // 延迟正常
  ]
  
  let alerts1 = evaluate_rules(engine, metrics1, 1300) // 当前时间1300，满足持续时间
  assert_eq(alerts1.length(), 1)
  assert_eq(alerts1[0].rule_name, "HighErrorRate")
  assert_eq(alerts1[0].severity, AlertSeverity::Critical)
  
  // 测试多条件告警
  let multi_condition_rule = {
    name: "ServiceDegradation",
    description: "服务性能下降",
    severity: AlertSeverity::Warning,
    conditions: [
      {
        metric_name: "error_rate",
        operator: ">",
        threshold: 0.01,
        duration: 300
      },
      {
        metric_name: "throughput",
        operator: "<",
        threshold: 100.0,
        duration: 300
      }
    ],
    labels: [
      ("team", "platform")
    ],
    annotations: [
      ("summary", "服务性能下降")
    ],
    enabled: true
  }
  
  let multi_engine = create_alert_engine([multi_condition_rule])
  
  // 只满足第一个条件
  let metrics2 = [
    ("error_rate", (0.02, 2000)), // 满足条件
    ("throughput", (150.0, 2000)) // 不满足条件
  ]
  
  let alerts2 = evaluate_rules(multi_engine, metrics2, 2300)
  assert_eq(alerts2.length(), 0) // 不应该触发告警
  
  // 满足所有条件
  let metrics3 = [
    ("error_rate", (0.02, 3000)), // 满足条件
    ("throughput", (80.0, 3000)) // 满足条件
  ]
  
  let alerts3 = evaluate_rules(multi_engine, metrics3, 3300)
  assert_eq(alerts3.length(), 1)
  assert_eq(alerts3[0].rule_name, "ServiceDegradation")
}

// 测试5: 自适应阈值调整
test "自适应阈值调整" {
  // 阈值调整策略
  enum ThresholdAdjustmentStrategy {
    NoAdjustment
    SeasonalAdjustment
    TrendBasedAdjustment
    AnomalyBasedAdjustment
  }
  
  // 自适应阈值配置
  type AdaptiveThresholdConfig = {
    metric_name: String,
    base_threshold: Float,
    adjustment_strategy: ThresholdAdjustmentStrategy,
    adjustment_factor: Float,
    min_threshold: Float,
    max_threshold: Float,
    learning_period: Int // 学习周期（秒）
  }
  
  // 阈值调整结果
  type ThresholdAdjustmentResult = {
    original_threshold: Float,
    adjusted_threshold: Float,
    adjustment_reason: String,
    confidence: Float
  }
  
  // 自适应阈值调整器
  let adjust_threshold = fn(config: AdaptiveThresholdConfig, historical_data: Array[(Float, Int)>, current_time: Int) {
    match config.adjustment_strategy {
      ThresholdAdjustmentStrategy::NoAdjustment => {
        {
          original_threshold: config.base_threshold,
          adjusted_threshold: config.base_threshold,
          adjustment_reason: "无调整策略",
          confidence: 1.0
        }
      }
      
      ThresholdAdjustmentStrategy::SeasonalAdjustment => {
        // 简化的季节性调整：基于历史数据的标准差
        if historical_data.length() < 10 {
          return {
            original_threshold: config.base_threshold,
            adjusted_threshold: config.base_threshold,
            adjustment_reason: "历史数据不足",
            confidence: 0.0
          }
        }
        
        let values = historical_data.map(fn(p) { p.0 })
        let mean = values.reduce(fn(acc, v) { acc + v }, 0.0) / (values.length() as Float)
        let variance = values.reduce(fn(acc, v) { acc + (v - mean) * (v - mean) }, 0.0) / (values.length() as Float)
        let std_dev = variance.sqrt()
        
        let adjusted = config.base_threshold + (std_dev * config.adjustment_factor)
        let clamped = adjusted.max(config.min_threshold).min(config.max_threshold)
        
        {
          original_threshold: config.base_threshold,
          adjusted_threshold: clamped,
          adjustment_reason: "基于历史标准差的季节性调整",
          confidence: 0.8
        }
      }
      
      ThresholdAdjustmentStrategy::TrendBasedAdjustment => {
        // 简化的趋势调整：基于最近数据的趋势
        if historical_data.length() < 5 {
          return {
            original_threshold: config.base_threshold,
            adjusted_threshold: config.base_threshold,
            adjustment_reason: "历史数据不足",
            confidence: 0.0
          }
        }
        
        let recent_data = historical_data.slice(historical_data.length() - 5)
        let first_value = recent_data[0].0
        let last_value = recent_data[recent_data.length() - 1].0
        let trend = (last_value - first_value) / (recent_data.length() as Float)
        
        let adjusted = config.base_threshold + (trend * config.adjustment_factor)
        let clamped = adjusted.max(config.min_threshold).min(config.max_threshold)
        
        {
          original_threshold: config.base_threshold,
          adjusted_threshold: clamped,
          adjustment_reason: "基于最近趋势的调整",
          confidence: 0.7
        }
      }
      
      ThresholdAdjustmentStrategy::AnomalyBasedAdjustment => {
        // 基于异常的调整：如果最近有异常，提高阈值
        let recent_data = historical_data.filter(fn(p) { current_time - p.1 < config.learning_period })
        
        if recent_data.length() < 3 {
          return {
            original_threshold: config.base_threshold,
            adjusted_threshold: config.base_threshold,
            adjustment_reason: "学习周期内数据不足",
            confidence: 0.0
          }
        }
        
        let values = recent_data.map(fn(p) { p.0 })
        let mean = values.reduce(fn(acc, v) { acc + v }, 0.0) / (values.length() as Float)
        
        let max_deviation = values.reduce(fn(acc, v) { 
          let deviation = (v - mean).abs()
          if deviation > acc { deviation } else { acc }
        }, 0.0)
        
        // 如果最大偏差较大，提高阈值
        let adjusted = if max_deviation > config.base_threshold * 0.5 {
          config.base_threshold * (1.0 + config.adjustment_factor)
        } else {
          config.base_threshold
        }
        
        let clamped = adjusted.max(config.min_threshold).min(config.max_threshold)
        
        {
          original_threshold: config.base_threshold,
          adjusted_threshold: clamped,
          adjustment_reason: "基于异常检测的调整",
          confidence: 0.6
        }
      }
    }
  }
  
  // 创建测试配置
  let seasonal_config = {
    metric_name: "response_time",
    base_threshold: 500.0,
    adjustment_strategy: ThresholdAdjustmentStrategy::SeasonalAdjustment,
    adjustment_factor: 1.5,
    min_threshold: 100.0,
    max_threshold: 1000.0,
    learning_period: 3600
  }
  
  let trend_config = {
    metric_name: "cpu_usage",
    base_threshold: 80.0,
    adjustment_strategy: ThresholdAdjustmentStrategy::TrendBasedAdjustment,
    adjustment_factor: 2.0,
    min_threshold: 50.0,
    max_threshold: 95.0,
    learning_period: 1800
  }
  
  let anomaly_config = {
    metric_name: "error_rate",
    base_threshold: 0.05,
    adjustment_strategy: ThresholdAdjustmentStrategy::AnomalyBasedAdjustment,
    adjustment_factor: 0.5,
    min_threshold: 0.01,
    max_threshold: 0.2,
    learning_period: 900
  }
  
  // 创建测试数据
  let stable_historical_data = [
    (450.0, 1000),
    (480.0, 2000),
    (470.0, 3000),
    (460.0, 4000),
    (490.0, 5000),
    (465.0, 6000),
    (475.0, 7000),
    (485.0, 8000),
    (455.0, 9000),
    (495.0, 10000)
  ]
  
  let increasing_trend_data = [
    (70.0, 1000),
    (72.0, 2000),
    (75.0, 3000),
    (78.0, 4000),
    (82.0, 5000)
  ]
  
  let anomaly_data = [
    (0.02, 1000),
    (0.03, 2000),
    (0.08, 3000), // 异常值
    (0.04, 4000),
    (0.03, 5000)
  ]
  
  // 测试季节性调整
  let seasonal_result = adjust_threshold(seasonal_config, stable_historical_data, 11000)
  assert_eq(seasonal_result.original_threshold, 500.0)
  assert_true(seasonal_result.adjusted_threshold != 500.0)
  assert_eq(seasonal_result.adjustment_reason, "基于历史标准差的季节性调整")
  assert_eq(seasonal_result.confidence, 0.8)
  
  // 测试趋势调整
  let trend_result = adjust_threshold(trend_config, increasing_trend_data, 6000)
  assert_eq(trend_result.original_threshold, 80.0)
  assert_true(trend_result.adjusted_threshold > 80.0) // 上升趋势应该提高阈值
  assert_eq(trend_result.adjustment_reason, "基于最近趋势的调整")
  assert_eq(trend_result.confidence, 0.7)
  
  // 测试异常调整
  let anomaly_result = adjust_threshold(anomaly_config, anomaly_data, 6000)
  assert_eq(anomaly_result.original_threshold, 0.05)
  assert_true(anomaly_result.adjusted_threshold > 0.05) // 异常应该提高阈值
  assert_eq(anomaly_result.adjustment_reason, "基于异常检测的调整")
  assert_eq(anomaly_result.confidence, 0.6)
  
  // 测试无调整策略
  let no_adjust_config = { ...seasonal_config, adjustment_strategy: ThresholdAdjustmentStrategy::NoAdjustment }
  let no_adjust_result = adjust_threshold(no_adjust_config, stable_historical_data, 11000)
  assert_eq(no_adjust_result.adjusted_threshold, no_adjust_result.original_threshold)
  assert_eq(no_adjust_result.adjustment_reason, "无调整策略")
  assert_eq(no_adjust_result.confidence, 1.0)
}