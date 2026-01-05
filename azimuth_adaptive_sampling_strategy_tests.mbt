// Azimuth 自适应采样策略测试
// 专注于智能采样策略的动态调整和优化

// 测试1: 基于负载的自适应采样
test "基于负载的自适应采样" {
  // 创建自适应采样管理器
  let adaptive_sampling_manager = AdaptiveSamplingManager::new()
  
  // 配置基于负载的采样策略
  AdaptiveSamplingManager::add_strategy(adaptive_sampling_manager, "load_based", {
    name: "Load-based Adaptive Sampling",
    description: "根据系统负载动态调整采样率",
    metrics: [
      { name: "cpu_usage", weight: 0.3, threshold: 80.0 },
      { name: "memory_usage", weight: 0.2, threshold: 85.0 },
      { name: "request_rate", weight: 0.3, threshold: 1000.0 },
      { name: "queue_depth", weight: 0.2, threshold: 100.0 }
    ],
    sampling_rate_range: { min: 0.01, max: 1.0 },
    adjustment_factor: 0.1,
    evaluation_interval: 30,  // 30秒评估一次
    stabilization_threshold: 0.05
  })
  
  // 创建负载模拟器
  let load_simulator = LoadSimulator::new()
  
  // 配置负载场景
  LoadSimulator::add_scenario(load_simulator, "normal_load", {
    name: "Normal Load",
    duration: 300,  // 5分钟
    cpu_usage: 45.0,
    memory_usage: 60.0,
    request_rate: 500.0,
    queue_depth: 20.0
  })
  
  LoadSimulator::add_scenario(load_simulator, "high_load", {
    name: "High Load",
    duration: 300,  // 5分钟
    cpu_usage: 85.0,
    memory_usage: 90.0,
    request_rate: 1500.0,
    queue_depth: 120.0
  })
  
  LoadSimulator::add_scenario(load_simulator, "extreme_load", {
    name: "Extreme Load",
    duration: 180,  // 3分钟
    cpu_usage: 95.0,
    memory_usage: 95.0,
    request_rate: 2500.0,
    queue_depth: 200.0
  })
  
  // 初始化采样率
  let initial_sampling_rate = 0.5
  AdaptiveSamplingManager::set_sampling_rate(adaptive_sampling_manager, "load_based", initial_sampling_rate)
  
  // 模拟正常负载场景
  let normal_load_result = LoadSimulator::run_scenario(load_simulator, "normal_load")
  
  // 应用自适应采样策略
  let normal_sampling_result = AdaptiveSamplingManager::evaluate_and_adjust(
    adaptive_sampling_manager, 
    "load_based", 
    normal_load_result.metrics
  )
  
  // 验证正常负载下的采样率调整
  assert_true(normal_sampling_result.adjustment_applied)
  assert_true(normal_sampling_result.new_sampling_rate >= initial_sampling_rate)
  assert_true(normal_sampling_result.new_sampling_rate <= 1.0)
  
  // 验证调整原因
  assert_true(normal_sampling_result.adjustment_reason.contains("load"))
  assert_true(normal_sampling_result.adjustment_factors.length() > 0)
  
  // 验证调整因子
  let cpu_factor = normal_sampling_result.adjustment_factors.find(fn(f) { f.metric == "cpu_usage" })
  assert_true(cpu_factor != None)
  
  match cpu_factor {
    Some(factor) => {
      assert_eq(factor.weight, 0.3)
      assert_true(factor.contribution >= 0.0)
    }
    None => assert_true(false)
  }
  
  // 模拟高负载场景
  let high_load_result = LoadSimulator::run_scenario(load_simulator, "high_load")
  
  // 应用自适应采样策略
  let high_sampling_result = AdaptiveSamplingManager::evaluate_and_adjust(
    adaptive_sampling_manager, 
    "load_based", 
    high_load_result.metrics
  )
  
  // 验证高负载下的采样率调整
  assert_true(high_sampling_result.adjustment_applied)
  assert_true(high_sampling_result.new_sampling_rate < normal_sampling_result.new_sampling_rate)
  assert_true(high_sampling_result.new_sampling_rate >= 0.01)  // 不低于最小采样率
  
  // 验证调整方向
  assert_eq(high_sampling_result.adjustment_direction, "decrease")
  assert_true(high_sampling_result.adjustment_magnitude > 0.1)
  
  // 模拟极端负载场景
  let extreme_load_result = LoadSimulator::run_scenario(load_simulator, "extreme_load")
  
  // 应用自适应采样策略
  let extreme_sampling_result = AdaptiveSamplingManager::evaluate_and_adjust(
    adaptive_sampling_manager, 
    "load_based", 
    extreme_load_result.metrics
  )
  
  // 验证极端负载下的采样率调整
  assert_true(extreme_sampling_result.adjustment_applied)
  assert_true(extreme_sampling_result.new_sampling_rate <= 0.1)  // 应该大幅降低采样率
  assert_eq(extreme_sampling_result.new_sampling_rate, 0.01)    // 应该达到最小采样率
  
  // 测试负载恢复场景
  let recovery_load_result = LoadSimulator::run_scenario(load_simulator, "normal_load")
  
  // 应用自适应采样策略
  let recovery_sampling_result = AdaptiveSamplingManager::evaluate_and_adjust(
    adaptive_sampling_manager, 
    "load_based", 
    recovery_load_result.metrics
  )
  
  // 验证负载恢复后的采样率调整
  assert_true(recovery_sampling_result.adjustment_applied)
  assert_true(recovery_sampling_result.new_sampling_rate > extreme_sampling_result.new_sampling_rate)
  assert_eq(recovery_sampling_result.adjustment_direction, "increase")
  
  // 测试采样率稳定性
  let stability_test_result = AdaptiveSamplingManager::test_sampling_stability(
    adaptive_sampling_manager,
    "load_based",
    {
      test_duration: 600,      // 10分钟
      metric_fluctuation: 0.1, // 10%波动
      stability_threshold: 0.05 // 5%稳定性阈值
    }
  )
  
  // 验证采样率稳定性
  assert_true(stability_test_result.is_stable)
  assert_true(stability_test_result.variance < 0.05)
  assert_true(stability_test_result.oscillation_count < 5)
  
  // 测试多策略协同
  AdaptiveSamplingManager::add_strategy(adaptive_sampling_manager, "error_based", {
    name: "Error-based Adaptive Sampling",
    description: "根据错误率动态调整采样率",
    metrics: [
      { name: "error_rate", weight: 0.6, threshold: 0.05 },
      { name: "timeout_rate", weight: 0.4, threshold: 0.02 }
    ],
    sampling_rate_range: { min: 0.1, max: 1.0 },
    adjustment_factor: 0.15,
    evaluation_interval: 15,
    stabilization_threshold: 0.03
  })
  
  // 测试策略协同
  let coordination_result = AdaptiveSamplingManager::test_strategy_coordination(
    adaptive_sampling_manager,
    ["load_based", "error_based"],
    {
      test_duration: 300,
      conflict_resolution: "min_sampling_rate",  // 取最小采样率
      priority_strategy: "load_based"
    }
  )
  
  // 验证策略协同
  assert_true(coordination_result.coordination_successful)
  assert_true(coordination_result.conflict_count > 0)
  assert_true(coordination_result.conflict_resolution_rate > 0.9)
  
  // 验证协同效果
  assert_true(coordination_result.final_sampling_rate >= 0.01)
  assert_true(coordination_result.final_sampling_rate <= 1.0)
}

// 测试2: 基于业务价值的自适应采样
test "基于业务价值的自适应采样" {
  // 创建业务价值采样管理器
  let business_value_sampling_manager = BusinessValueSamplingManager::new()
  
  // 配置业务规则
  BusinessValueSamplingManager::add_business_rule(business_value_sampling_manager, "high_value_customers", {
    name: "High Value Customers",
    description: "对高价值客户的请求进行更高比例采样",
    conditions: [
      { field: "customer.tier", operator: "in", values: ["premium", "enterprise"] },
      { field: "user.subscription_level", operator: "=", value: "premium" }
    ],
    sampling_multiplier: 2.0,
    priority: "high"
  })
  
  BusinessValueSamplingManager::add_business_rule(business_value_sampling_manager, "critical_operations", {
    name: "Critical Operations",
    description: "对关键业务操作进行更高比例采样",
    conditions: [
      { field: "operation.type", operator: "in", values: ["payment", "checkout", "user_registration"] },
      { field: "transaction.value", operator: ">", value: 100.0 }
    ],
    sampling_multiplier: 3.0,
    priority: "critical"
  })
  
  BusinessValueSamplingManager::add_business_rule(business_value_sampling_manager, "error_scenarios", {
    name: "Error Scenarios",
    description: "对错误场景进行全采样",
    conditions: [
      { field: "http.status_code", operator: "in", values: [400, 401, 403, 404, 500, 502, 503, 504] },
      { field: "error.type", operator: "exists" }
    ],
    sampling_multiplier: 10.0,
    priority: "critical"
  })
  
  BusinessValueSamplingManager::add_business_rule(business_value_sampling_manager, "low_priority_operations", {
    name: "Low Priority Operations",
    description: "对低优先级操作进行较低比例采样",
    conditions: [
      { field: "operation.type", operator: "in", values: ["health_check", "metrics_collection", "log_aggregation"] },
      { field: "system.operation", operator: "=", value: "true" }
    ],
    sampling_multiplier: 0.1,
    priority: "low"
  })
  
  // 创建业务场景模拟器
  let business_scenario_simulator = BusinessScenarioSimulator::new()
  
  // 配置业务场景
  BusinessScenarioSimulator::add_scenario(business_scenario_simulator, "normal_business_hours", {
    name: "Normal Business Hours",
    duration: 3600,  // 1小时
    request_distribution: {
      high_value_customers: 0.2,    // 20%高价值客户
      critical_operations: 0.1,     // 10%关键操作
      error_scenarios: 0.02,        // 2%错误场景
      low_priority_operations: 0.3  // 30%低优先级操作
    },
    base_sampling_rate: 0.1
  })
  
  BusinessScenarioSimulator::add_scenario(business_scenario_simulator, "peak_hours", {
    name: "Peak Hours",
    duration: 1800,  // 30分钟
    request_distribution: {
      high_value_customers: 0.3,    // 30%高价值客户
      critical_operations: 0.15,    // 15%关键操作
      error_scenarios: 0.05,        // 5%错误场景
      low_priority_operations: 0.1  // 10%低优先级操作
    },
    base_sampling_rate: 0.05
  })
  
  BusinessScenarioSimulator::add_scenario(business_scenario_simulator, "flash_sale", {
    name: "Flash Sale Event",
    duration: 900,   // 15分钟
    request_distribution: {
      high_value_customers: 0.4,    // 40%高价值客户
      critical_operations: 0.3,     // 30%关键操作
      error_scenarios: 0.1,         // 10%错误场景
      low_priority_operations: 0.05 // 5%低优先级操作
    },
    base_sampling_rate: 0.02
  })
  
  // 模拟正常营业时间场景
  let normal_business_result = BusinessScenarioSimulator::run_scenario(
    business_scenario_simulator,
    "normal_business_hours"
  )
  
  // 应用业务价值采样策略
  let normal_business_sampling_result = BusinessValueSamplingManager::apply_business_rules(
    business_value_sampling_manager,
    normal_business_result.requests
  )
  
  // 验证正常营业时间的采样结果
  assert_eq(normal_business_sampling_result.total_requests, normal_business_result.requests.length())
  assert_true(normal_business_sampling_result.sampled_requests > 0)
  assert_true(normal_business_sampling_result.sampling_rate > normal_business_result.base_sampling_rate)
  
  // 验证高价值客户采样
  let high_value_sampled = normal_business_sampling_result.categorized_samples["high_value_customers"]
  let high_value_total = normal_business_sampling_result.categorized_totals["high_value_customers"]
  
  assert_true(high_value_total > 0)
  assert_true(high_value_sampled > 0)
  
  let high_value_actual_rate = high_value_sampled.to_float() / high_value_total.to_float()
  assert_true(high_value_actual_rate > normal_business_result.base_sampling_rate * 2.0)
  
  // 验证关键操作采样
  let critical_operations_sampled = normal_business_sampling_result.categorized_samples["critical_operations"]
  let critical_operations_total = normal_business_sampling_result.categorized_totals["critical_operations"]
  
  assert_true(critical_operations_total > 0)
  assert_true(critical_operations_sampled > 0)
  
  let critical_operations_actual_rate = critical_operations_sampled.to_float() / critical_operations_total.to_float()
  assert_true(critical_operations_actual_rate > normal_business_result.base_sampling_rate * 3.0)
  
  // 验证错误场景采样
  let error_scenarios_sampled = normal_business_sampling_result.categorized_samples["error_scenarios"]
  let error_scenarios_total = normal_business_sampling_result.categorized_totals["error_scenarios"]
  
  assert_true(error_scenarios_total > 0)
  assert_eq(error_scenarios_sampled, error_scenarios_total)  // 错误场景应该全采样
  
  // 验证低优先级操作采样
  let low_priority_sampled = normal_business_sampling_result.categorized_samples["low_priority_operations"]
  let low_priority_total = normal_business_sampling_result.categorized_totals["low_priority_operations"]
  
  assert_true(low_priority_total > 0)
  assert_true(low_priority_sampled > 0)
  
  let low_priority_actual_rate = low_priority_sampled.to_float() / low_priority_total.to_float()
  assert_true(low_priority_actual_rate < normal_business_result.base_sampling_rate)
  
  // 模拟高峰时段场景
  let peak_hours_result = BusinessScenarioSimulator::run_scenario(
    business_scenario_simulator,
    "peak_hours"
  )
  
  // 应用业务价值采样策略
  let peak_hours_sampling_result = BusinessValueSamplingManager::apply_business_rules(
    business_value_sampling_manager,
    peak_hours_result.requests
  )
  
  // 验证高峰时段的采样结果
  assert_true(peak_hours_sampling_result.sampling_rate > peak_hours_result.base_sampling_rate)
  
  // 验证资源使用效率
  let resource_efficiency = BusinessValueSamplingManager::calculate_resource_efficiency(
    business_value_sampling_manager,
    peak_hours_sampling_result
  )
  
  assert_true(resource_efficiency.business_value_ratio > 0.7)  // 70%以上的采样价值
  assert_true(resource_efficiency.resource_utilization < 0.8)  // 资源利用率不超过80%
  
  // 模拟闪购活动场景
  let flash_sale_result = BusinessScenarioSimulator::run_scenario(
    business_scenario_simulator,
    "flash_sale"
  )
  
  // 应用业务价值采样策略
  let flash_sale_sampling_result = BusinessValueSamplingManager::apply_business_rules(
    business_value_sampling_manager,
    flash_sale_result.requests
  )
  
  // 验证闪购活动的采样结果
  assert_true(flash_sale_sampling_result.sampling_rate > flash_sale_result.base_sampling_rate)
  
  // 验证关键业务覆盖率
  let business_coverage = BusinessValueSamplingManager::calculate_business_coverage(
    business_value_sampling_manager,
    flash_sale_sampling_result
  )
  
  assert_true(business_coverage.critical_operations_coverage > 0.9)  // 关键操作覆盖率超过90%
  assert_true(business_coverage.high_value_customers_coverage > 0.8) // 高价值客户覆盖率超过80%
  assert_true(business_coverage.error_scenarios_coverage > 0.99)     // 错误场景覆盖率超过99%
  
  // 测试业务规则优先级
  let priority_test_result = BusinessValueSamplingManager::test_rule_priority(
    business_value_sampling_manager,
    {
      // 创建一个同时匹配多个规则的请求
      customer_tier: "premium",
      operation_type: "payment",
      http_status_code: 500,
      transaction_value: 200.0
    }
  )
  
  // 验证规则优先级
  assert_eq(priority_test_result.applied_rules.length(), 3)  // 应该匹配3个规则
  assert_eq(priority_test_result.final_sampling_multiplier, 10.0)  // 应该应用最高优先级规则（错误场景）
  assert_eq(priority_test_result.priority_rule, "error_scenarios")
  
  // 测试动态业务规则调整
  let dynamic_adjustment_result = BusinessValueSamplingManager::test_dynamic_adjustment(
    business_value_sampling_manager,
    {
      adjustment_period: 1800,  // 30分钟
      business_metrics_tracking: true,
      feedback_loop_enabled: true
    }
  )
  
  // 验证动态调整
  assert_true(dynamic_adjustment_result.adjustment_applied)
  assert_true(dynamic_adjustment_result.rule_efficiency_improvement > 0.1)  // 规则效率提升超过10%
  assert_true(dynamic_adjustment_result.business_value_optimized)
  
  // 测试业务价值采样与成本平衡
  let cost_balance_result = BusinessValueSamplingManager::test_cost_balance(
    business_value_sampling_manager,
    {
      budget_constraints: {
        max_spans_per_hour: 10000,
        max_storage_gb_per_day: 50.0,
        max_network_bandwidth_mbps: 10.0
      },
      business_objectives: {
        min_critical_operations_coverage: 0.9,
        min_high_value_customers_coverage: 0.8,
        min_error_scenarios_coverage: 0.95
      }
    }
  )
  
  // 验证成本平衡
  assert_true(cost_balance_result.constraints_satisfied)
  assert_true(cost_balance_result.objectives_met)
  assert_true(cost_balance_result.cost_efficiency_score > 0.8)
  
  // 验证资源分配
  assert_true(cost_balance_result.resource_allocation.critical_operations > 0.5)
  assert_true(cost_balance_result.resource_allocation.error_scenarios > 0.3)
  assert_true(cost_balance_result.resource_allocation.high_value_customers > 0.15)
}

// 测试3: 基于机器学习的智能采样
test "基于机器学习的智能采样" {
  // 创建机器学习采样管理器
  let ml_sampling_manager = MLSamplingManager::new()
  
  // 配置ML模型
  MLSamplingManager::add_model(ml_sampling_manager, "anomaly_detection", {
    name: "Anomaly Detection Model",
    type: "isolation_forest",
    features: [
      "request_duration",
      "response_size",
      "cpu_usage",
      "memory_usage",
      "error_rate",
      "time_of_day",
      "day_of_week"
    ],
    target: "is_anomaly",
    training_data_size: 10000,
    retraining_interval: 86400,  // 24小时重新训练
    anomaly_threshold: 0.1
  })
  
  MLSamplingManager::add_model(ml_sampling_manager, "business_impact", {
    name: "Business Impact Model",
    type: "gradient_boosting",
    features: [
      "user_tier",
      "operation_type",
      "transaction_value",
      "conversion_probability",
      "customer_lifetime_value"
    ],
    target: "business_impact_score",
    training_data_size: 5000,
    retraining_interval: 604800,  // 7天重新训练
    impact_threshold: 0.7
  })
  
  MLSamplingManager::add_model(ml_sampling_manager, "performance_prediction", {
    name: "Performance Prediction Model",
    type: "lstm",
    features: [
      "historical_response_time",
      "current_load",
      "queue_depth",
      "database_connections",
      "cache_hit_rate"
    ],
    target: "predicted_response_time",
    training_data_size: 20000,
    retraining_interval: 43200,  // 12小时重新训练
    performance_threshold: 500.0
  })
  
  // 创建训练数据生成器
  let training_data_generator = TrainingDataGenerator::new()
  
  // 生成异常检测训练数据
  let anomaly_training_data = training_data_generator.generate_anomaly_data({
    normal_samples: 9000,
    anomaly_samples: 1000,
    feature_ranges: {
      request_duration: [10, 1000],
      response_size: [100, 10000],
      cpu_usage: [10, 90],
      memory_usage: [20, 80],
      error_rate: [0, 0.1],
      time_of_day: [0, 23],
      day_of_week: [0, 6]
    }
  })
  
  // 训练异常检测模型
  let anomaly_training_result = MLSamplingManager::train_model(
    ml_sampling_manager,
    "anomaly_detection",
    anomaly_training_data
  )
  
  // 验证异常检测模型训练
  assert_true(anomaly_training_result.training_successful)
  assert_true(anomaly_training_result.model_accuracy > 0.9)
  assert_true(anomaly_training_result.training_time_ms < 60000)  // 训练时间小于1分钟
  
  // 生成业务影响训练数据
  let business_impact_training_data = training_data_generator.generate_business_impact_data({
    sample_count: 5000,
    user_tiers: ["basic", "premium", "enterprise"],
    operation_types: ["browse", "search", "add_to_cart", "checkout", "payment"],
    transaction_value_range: [0, 1000],
    conversion_probability_range: [0, 1],
    customer_lifetime_value_range: [100, 10000]
  })
  
  // 训练业务影响模型
  let business_impact_training_result = MLSamplingManager::train_model(
    ml_sampling_manager,
    "business_impact",
    business_impact_training_data
  )
  
  // 验证业务影响模型训练
  assert_true(business_impact_training_result.training_successful)
  assert_true(business_impact_training_result.model_accuracy > 0.85)
  assert_true(business_impact_training_result.feature_importance.length() > 0)
  
  // 验证特征重要性
  let transaction_value_importance = business_impact_training_result.feature_importance.find(
    fn(f) { f.feature == "transaction_value" }
  )
  assert_true(transaction_value_importance != None)
  
  match transaction_value_importance {
    Some(importance) => {
      assert_true(importance.importance_score > 0.2)  // 交易价值应该是一个重要特征
    }
    None => assert_true(false)
  }
  
  // 生成性能预测训练数据
  let performance_training_data = training_data_generator.generate_performance_data({
    sample_count: 20000,
    time_series_length: 24,  // 24小时时间序列
    feature_ranges: {
      historical_response_time: [50, 500],
      current_load: [100, 1000],
      queue_depth: [10, 100],
      database_connections: [10, 50],
      cache_hit_rate: [0.7, 0.95]
    }
  })
  
  // 训练性能预测模型
  let performance_training_result = MLSamplingManager::train_model(
    ml_sampling_manager,
    "performance_prediction",
    performance_training_data
  )
  
  // 验证性能预测模型训练
  assert_true(performance_training_result.training_successful)
  assert_true(performance_training_result.mean_absolute_error < 50.0)  // MAE小于50ms
  assert_true(performance_training_result.training_time_ms < 300000)  // 训练时间小于5分钟
  
  // 创建实时推理引擎
  let inference_engine = MLInferenceEngine::new()
  
  // 配置推理引擎
  MLInferenceEngine::configure(inference_engine, {
    batch_size: 100,
    max_latency_ms: 10,
    model_cache_size: 3,
    fallback_strategy: "rule_based"
  })
  
  // 加载训练好的模型
  MLInferenceEngine::load_model(inference_engine, ml_sampling_manager.get_model("anomaly_detection"))
  MLInferenceEngine::load_model(inference_engine, ml_sampling_manager.get_model("business_impact"))
  MLInferenceEngine::load_model(inference_engine, ml_sampling_manager.get_model("performance_prediction"))
  
  // 创建测试请求流
  let test_requests = []
  let base_time = 1640995200
  
  for i in 0..=1000 {
    let request = {
      timestamp: base_time + i * 10,
      request_id: "req-" + i.to_string(),
      user_tier: ["basic", "premium", "enterprise"][i % 3],
      operation_type: ["browse", "search", "add_to_cart", "checkout", "payment"][i % 5],
      transaction_value: if i % 10 == 0 { 500.0 } else { 50.0 },
      request_duration: if i % 50 == 0 { 800.0 } else { 100.0 + (i % 50) * 2.0 },
      response_size: 1000 + (i % 100) * 10,
      cpu_usage: 30.0 + (i % 20) * 2.0,
      memory_usage: 50.0 + (i % 15) * 2.0,
      error_rate: if i % 100 == 0 { 0.05 } else { 0.001 },
      time_of_day: (base_time + i * 10) / 3600 % 24,
      day_of_week: ((base_time + i * 10) / 86400) % 7,
      current_load: 200.0 + (i % 30) * 10.0,
      queue_depth: 5 + (i % 10),
      database_connections: 15 + (i % 5),
      cache_hit_rate: 0.8 + (i % 10) * 0.01
    }
    
    test_requests = test_requests.push(request)
  }
  
  // 执行实时推理
  let inference_result = MLInferenceEngine::batch_infer(inference_engine, test_requests)
  
  // 验证推理结果
  assert_eq(inference_result.total_requests, 1001)
  assert_eq(inference_result.successful_inferences, 1001)
  assert_eq(inference_result.failed_inferences, 0)
  assert_true(inference_result.average_inference_time_ms < 10.0)
  
  // 验证异常检测结果
  let anomaly_predictions = inference_result.predictions["anomaly_detection"]
  assert_eq(anomaly_predictions.length(), 1001)
  
  let anomaly_count = anomaly_predictions.filter(fn(p) { p.prediction > 0.5 }).length()
  assert_true(anomaly_count > 0)  // 应该检测到一些异常
  assert_true(anomaly_count < 100)  // 但不应该太多
  
  // 验证业务影响预测
  let business_impact_predictions = inference_result.predictions["business_impact"]
  assert_eq(business_impact_predictions.length(), 1001)
  
  let high_impact_count = business_impact_predictions.filter(fn(p) { p.prediction > 0.7 }).length()
  assert_true(high_impact_count > 0)
  
  // 验证性能预测
  let performance_predictions = inference_result.predictions["performance_prediction"]
  assert_eq(performance_predictions.length(), 1001)
  
  let high_latency_predictions = performance_predictions.filter(fn(p) { p.prediction > 500.0 }).length()
  assert_true(high_latency_predictions > 0)
  
  // 应用智能采样策略
  let intelligent_sampling_result = MLSamplingManager::apply_intelligent_sampling(
    ml_sampling_manager,
    inference_result,
    {
      base_sampling_rate: 0.1,
      anomaly_sampling_multiplier: 5.0,
      high_impact_sampling_multiplier: 3.0,
      high_latency_sampling_multiplier: 2.0,
      max_sampling_rate: 1.0,
      min_sampling_rate: 0.01
    }
  )
  
  // 验证智能采样结果
  assert_eq(intelligent_sampling_result.total_requests, 1001)
  assert_true(intelligent_sampling_result.sampled_requests > 100)  // 应该采样超过100个请求
  assert_true(intelligent_sampling_result.sampling_rate > 0.1)    // 应该高于基础采样率
  
  // 验证异常请求采样
  let anomaly_requests = intelligent_sampling_result.categorized_samples["anomalous"]
  let anomaly_total = intelligent_sampling_result.categorized_totals["anomalous"]
  
  if anomaly_total > 0 {
    let anomaly_sampling_rate = anomaly_requests.to_float() / anomaly_total.to_float()
    assert_true(anomaly_sampling_rate > 0.5)  // 异常请求应该有高采样率
  }
  
  // 验证高业务影响请求采样
  let high_impact_requests = intelligent_sampling_result.categorized_samples["high_impact"]
  let high_impact_total = intelligent_sampling_result.categorized_totals["high_impact"]
  
  if high_impact_total > 0 {
    let high_impact_sampling_rate = high_impact_requests.to_float() / high_impact_total.to_float()
    assert_true(high_impact_sampling_rate > 0.3)  // 高业务影响请求应该有较高采样率
  }
  
  // 测试模型性能监控
  let model_monitoring_result = MLSamplingManager::monitor_model_performance(
    ml_sampling_manager,
    {
      monitoring_period: 3600,  // 1小时
      performance_threshold: 0.8,
      drift_threshold: 0.2,
      accuracy_degradation_threshold: 0.1
    }
  )
  
  // 验证模型性能监控
  assert_true(model_monitoring_result.monitoring_active)
  assert_true(model_monitoring_result.models_monitored.length() == 3)
  
  for model_monitor in model_monitoring_result.models_monitored {
    assert_true(model_monitor.current_accuracy > 0.7)
    assert_true(model_monitor.drift_detected == false or model_monitor.drift_detected == true)  // 可能检测到漂移
    assert_true(model_monitor.performance_degradation < 0.2)
  }
  
  // 测试模型自动重训练
  let retraining_result = MLSamplingManager::test_automatic_retraining(
    ml_sampling_manager,
    {
      drift_threshold: 0.15,
      accuracy_threshold: 0.8,
      min_samples_for_retraining: 1000,
      retraining_schedule: "weekly"
    }
  )
  
  // 验证自动重训练
  assert_true(retraining_result.retraining_triggered or retraining_result.retraining_not_needed)
  
  if retraining_result.retraining_triggered {
    assert_true(retraining_result.retraining_successful)
    assert_true(retraining_result.new_model_accuracy > retraining_result.old_model_accuracy)
  }
  
  // 测试A/B测试框架
  let ab_test_result = MLSamplingManager::run_ab_test(
    ml_sampling_manager,
    {
      test_name: "ml_sampling_vs_rule_based",
      control_group: {
        name: "rule_based_sampling",
        sampling_strategy: "rule_based",
        base_sampling_rate: 0.1
      },
      test_group: {
        name: "ml_intelligent_sampling",
        sampling_strategy: "ml_intelligent",
        base_sampling_rate: 0.1
      },
      traffic_split: 0.5,  // 50%流量分配给测试组
      test_duration: 3600,  // 1小时测试
      success_metrics: [
        "anomaly_detection_rate",
        "business_impact_coverage",
        "cost_efficiency"
      ]
    }
  )
  
  // 验证A/B测试结果
  assert_true(ab_test_result.test_completed)
  assert_true(ab_test_result.control_group_metrics.length() > 0)
  assert_true(ab_test_result.test_group_metrics.length() > 0)
  
  // 验证测试结果
  let control_anomaly_rate = ab_test_result.control_group_metrics.find(
    fn(m) { m.name == "anomaly_detection_rate" }
  )
  let test_anomaly_rate = ab_test_result.test_group_metrics.find(
    fn(m) { m.name == "anomaly_detection_rate" }
  )
  
  assert_true(control_anomaly_rate != None and test_anomaly_rate != None)
  
  match (control_anomaly_rate, test_anomaly_rate) {
    (Some(control), Some(test)) => {
      // ML采样应该有更高的异常检测率
      assert_true(test.value > control.value)
    }
    _ => assert_true(false)
  }
  
  // 验证统计显著性
  assert_true(ab_test_result.statistical_significance > 0.95)
  assert_true(ab_test_result.confidence_interval_lower > 0)
  assert_true(ab_test_result.confidence_interval_upper > ab_test_result.confidence_interval_lower)
}

// 测试4: 多层次自适应采样策略
test "多层次自适应采样策略" {
  // 创建多层次采样管理器
  let multi_level_sampling_manager = MultiLevelSamplingManager::new()
  
  // 配置采样层次
  MultiLevelSamplingManager::add_level(multi_level_sampling_manager, "edge", {
    name: "Edge Sampling",
    description: "在边缘层进行的初步采样",
    position: 1,
    strategies: [
      {
        name: "simple_random",
        type: "probabilistic",
        sampling_rate: 0.8,
        adaptive: false
      },
      {
        name: "error_prioritization",
        type: "rule_based",
        rules: [
          { condition: "status_code >= 400", action: "always_sample" },
          { condition: "response_time > 1000", action: "always_sample" }
        ]
      }
    ],
    max_sampling_rate: 1.0,
    min_sampling_rate: 0.1
  })
  
  MultiLevelSamplingManager::add_level(multi_level_sampling_manager, "service", {
    name: "Service Sampling",
    description: "在服务层进行的二次采样",
    position: 2,
    strategies: [
      {
        name: "business_value",
        type: "ml_based",
        model: "business_impact_model",
        sampling_rate_range: [0.1, 1.0]
      },
      {
        name: "load_aware",
        type: "adaptive",
        metrics: ["cpu_usage", "memory_usage", "request_rate"],
        target_sampling_rate: 0.3
      }
    ],
    max_sampling_rate: 0.8,
    min_sampling_rate: 0.05
  })
  
  MultiLevelSamplingManager::add_level(multi_level_sampling_manager, "collector", {
    name: "Collector Sampling",
    description: "在收集器层进行的最终采样",
    position: 3,
    strategies: [
      {
        name: "storage_optimization",
        type: "cost_based",
        storage_budget_gb: 100.0,
        retention_days: 30,
        target_sampling_rate: 0.2
      },
      {
        name: "query_optimization",
        type: "usage_based",
        query_frequency_weight: 0.6,
        business_value_weight: 0.4,
        target_sampling_rate: 0.15
      }
    ],
    max_sampling_rate: 0.5,
    min_sampling_rate: 0.01
  })
  
  // 创建多层次请求模拟器
  let multi_level_simulator = MultiLevelSimulator::new()
  
  // 配置请求流
  MultiLevelSimulator::configure_request_flow(multi_level_simulator, {
    total_requests: 10000,
    request_types: [
      { type: "user_request", proportion: 0.7 },
      { type: "system_request", proportion: 0.2 },
      { type: "health_check", proportion: 0.1 }
    ],
    error_rate: 0.02,
    high_latency_rate: 0.05,
    business_value_distribution: {
      high: 0.1,
      medium: 0.3,
      low: 0.6
    }
  })
  
  // 模拟请求通过多层次采样
  let multi_level_result = MultiLevelSimulator::run_simulation(
    multi_level_simulator,
    multi_level_sampling_manager
  )
  
  // 验证多层次采样结果
  assert_eq(multi_level_result.total_requests, 10000)
  assert_true(multi_level_result.edge_sampled_requests > 8000)   // 边缘层应该采样大部分
  assert_true(multi_level_result.service_sampled_requests > 2000) // 服务层应该采样较少
  assert_true(multi_level_result.collector_sampled_requests > 500) // 收集器层应该采样最少
  
  // 验证采样率递减
  let edge_sampling_rate = multi_level_result.edge_sampled_requests.to_float() / 10000.0
  let service_sampling_rate = multi_level_result.service_sampled_requests.to_float() / multi_level_result.edge_sampled_requests.to_float()
  let collector_sampling_rate = multi_level_result.collector_sampled_requests.to_float() / multi_level_result.service_sampled_requests.to_float()
  
  assert_true(edge_sampling_rate > service_sampling_rate)
  assert_true(service_sampling_rate > collector_sampling_rate)
  
  // 验证错误请求采样
  let error_requests = multi_level_result.categorized_requests["error"]
  let error_sampled_at_edge = multi_level_result.edge_categorized_samples["error"]
  let error_sampled_at_service = multi_level_result.service_categorized_samples["error"]
  let error_sampled_at_collector = multi_level_result.collector_categorized_samples["error"]
  
  if error_requests > 0 {
    // 错误请求应该在边缘层全采样
    assert_eq(error_sampled_at_edge, error_requests)
    
    // 错误请求应该在各层都保持高采样率
    let edge_error_rate = error_sampled_at_edge.to_float() / error_requests.to_float()
    let service_error_rate = error_sampled_at_service.to_float() / error_sampled_at_edge.to_float()
    let collector_error_rate = error_sampled_at_collector.to_float() / error_sampled_at_service.to_float()
    
    assert_true(edge_error_rate >= 0.9)
    assert_true(service_error_rate >= 0.8)
    assert_true(collector_error_rate >= 0.7)
  }
  
  // 验证高业务价值请求采样
  let high_value_requests = multi_level_result.categorized_requests["high_business_value"]
  let high_value_sampled_at_edge = multi_level_result.edge_categorized_samples["high_business_value"]
  let high_value_sampled_at_service = multi_level_result.service_categorized_samples["high_business_value"]
  let high_value_sampled_at_collector = multi_level_result.collector_categorized_samples["high_business_value"]
  
  if high_value_requests > 0 {
    // 高业务价值请求应该在各层都保持高采样率
    let edge_high_value_rate = high_value_sampled_at_edge.to_float() / high_value_requests.to_float()
    let service_high_value_rate = high_value_sampled_at_service.to_float() / high_value_sampled_at_edge.to_float()
    let collector_high_value_rate = high_value_sampled_at_collector.to_float() / high_value_sampled_at_service.to_float()
    
    assert_true(edge_high_value_rate >= 0.8)
    assert_true(service_high_value_rate >= 0.7)
    assert_true(collector_high_value_rate >= 0.6)
  }
  
  // 测试层次间协调
  let coordination_result = MultiLevelSamplingManager::test_level_coordination(
    multi_level_sampling_manager,
    {
      coordination_strategy: "feedback_loop",
      feedback_interval: 60,  // 1分钟反馈间隔
      adjustment_factor: 0.1,
      stability_threshold: 0.05
    }
  )
  
  // 验证层次间协调
  assert_true(coordination_result.coordination_active)
  assert_true(coordination_result.feedback_loops_established == 3)  // 3个层次间应该有反馈循环
  assert_true(coordination_result.adjustment_frequency > 0)
  
  // 验证协调效果
  assert_true(coordination_result.sampling_rate_stability > 0.8)  // 采样率稳定性大于80%
  assert_true(coordination_result.resource_utilization_optimized)
  assert_true(coordination_result.business_value_preserved)
  
  // 测试多层次采样策略优化
  let optimization_result = MultiLevelSamplingManager::optimize_sampling_strategy(
    multi_level_sampling_manager,
    {
      optimization_objective: "maximize_business_value",
      constraints: [
        { type: "storage", limit: 100.0, unit: "GB" },
        { type: "network", limit: 10.0, unit: "Mbps" },
        { type: "cpu", limit: 20.0, unit: "percent" }
      ],
      optimization_horizon: 3600,  // 1小时优化窗口
      convergence_threshold: 0.01
    }
  )
  
  // 验证优化结果
  assert_true(optimization_result.optimization_completed)
  assert_true(optimization_result.converged)
  assert_true(optimization_result.iterations < 100)  // 应该在100次迭代内收敛
  
  // 验证优化后的采样率
  let optimized_rates = optimization_result.optimized_sampling_rates
  assert_eq(optimized_rates.length(), 3)
  
  assert_true(optimized_rates[0].level == "edge")
  assert_true(optimized_rates[1].level == "service")
  assert_true(optimized_rates[2].level == "collector")
  
  // 验证优化效果
  assert_true(optimization_result.business_value_improvement > 0.1)  // 业务价值提升超过10%
  assert_true(optimization_result.cost_reduction > 0.05)           // 成本降低超过5%
  
  // 测试多层次采样故障恢复
  let fault_tolerance_result = MultiLevelSamplingManager::test_fault_tolerance(
    multi_level_sampling_manager,
    {
      fault_scenarios: [
        {
          level: "edge",
          fault_type: "high_load",
          duration: 300,  // 5分钟
          severity: "medium"
        },
        {
          level: "service",
          fault_type: "model_failure",
          duration: 180,  // 3分钟
          severity: "high"
        },
        {
          level: "collector",
          fault_type: "storage_full",
          duration: 120,  // 2分钟
          severity: "critical"
        }
      ],
      recovery_strategies: [
        { level: "edge", strategy: "reduce_sampling_rate" },
        { level: "service", strategy: "fallback_to_rules" },
        { level: "collector", strategy: "emergency_sampling" }
      ]
    }
  )
  
  // 验证故障恢复
  assert_true(fault_tolerance_result.all_faults_handled)
  assert_true(fault_tolerance_result.average_recovery_time_ms < 30000)  // 平均恢复时间小于30秒
  assert_true(fault_tolerance_result.data_loss_prevented)
  
  // 验证故障期间的性能
  assert_true(fault_tolerance_result.fault_period_availability > 0.9)  // 故障期间可用性大于90%
  assert_true(fault_tolerance_result.sampling_continuity_maintained)
  
  // 测试多层次采样性能监控
  let performance_monitoring_result = MultiLevelSamplingManager::monitor_performance(
    multi_level_sampling_manager,
    {
      monitoring_duration: 3600,  // 1小时
      metrics_collection_interval: 60,  // 1分钟收集一次指标
      performance_thresholds: {
        max_latency_ms: 50,
        max_cpu_usage_percent: 30,
        max_memory_usage_mb: 512,
        min_throughput_requests_per_sec: 100
      }
    }
  )
  
  // 验证性能监控
  assert_true(performance_monitoring_result.monitoring_active)
  assert_eq(performance_monitoring_result.level_metrics.length(), 3)
  
  // 验证各层性能
  for level_metrics in performance_monitoring_result.level_metrics {
    assert_true(level_metrics.average_latency_ms < 50)
    assert_true(level_metrics.max_cpu_usage_percent < 30)
    assert_true(level_metrics.max_memory_usage_mb < 512)
    assert_true(level_metrics.min_throughput_requests_per_sec > 100)
  }
  
  // 验证整体性能
  assert_true(performance_monitoring_result.overall_latency_ms < 100)
  assert_true(performance_monitoring_result.overall_cpu_usage_percent < 50)
  assert_true(performance_monitoring_result.overall_memory_usage_mb < 1024)
  assert_true(performance_monitoring_result.overall_throughput_requests_per_sec > 200)
  
  // 生成多层次采样报告
  let multi_level_report = MultiLevelSamplingManager::generate_comprehensive_report(
    multi_level_sampling_manager,
    multi_level_result,
    coordination_result,
    optimization_result,
    fault_tolerance_result,
    performance_monitoring_result
  )
  
  // 验证多层次采样报告
  assert_true(multi_level_report.executive_summary.length() > 0)
  assert_true(multi_level_report.level_performance_analysis.length() > 0)
  assert_true(multi_level_report.coordination_effectiveness_analysis.length() > 0)
  assert_true(multi_level_report.optimization_results_analysis.length() > 0)
  assert_true(multi_level_report.fault_tolerance_analysis.length() > 0)
  assert_true(multi_level_report.recommendations.length() > 0)
  
  // 验证建议内容
  let performance_recommendations = multi_level_report.recommendations.filter(
    fn(r) { r.category == "performance" }
  )
  assert_true(performance_recommendations.length() > 0)
  
  let coordination_recommendations = multi_level_report.recommendations.filter(
    fn(r) { r.category == "coordination" }
  )
  assert_true(coordination_recommendations.length() > 0)
  
  let optimization_recommendations = multi_level_report.recommendations.filter(
    fn(r) { r.category == "optimization" }
  )
  assert_true(optimization_recommendations.length() > 0)
}