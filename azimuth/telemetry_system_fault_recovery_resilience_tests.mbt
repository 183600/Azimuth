// 遥测系统故障恢复和容错测试
// 测试故障检测、自动恢复、降级策略和系统弹性

pub test "故障检测和诊断测试" {
  let resilience_manager = azimuth::ResilienceManager::new()
  
  // 创建故障检测管理器
  let fault_detector = azimuth::ResilienceManager::create_fault_detector(resilience_manager)
  
  // 配置故障检测规则
  let detection_rules = [
    {
      name: "high_error_rate",
      condition: "error_rate > 0.1",
      evaluation_window_seconds: 60,
      consecutive_breaches: 3,
      severity: "warning"
    },
    {
      name: "high_latency",
      condition: "p95_latency > 1000",
      evaluation_window_seconds: 30,
      consecutive_breaches: 2,
      severity: "critical"
    },
    {
      name: "service_unavailable",
      condition: "availability < 0.95",
      evaluation_window_seconds: 120,
      consecutive_breaches: 1,
      severity: "critical"
    },
    {
      name: "memory_pressure",
      condition: "memory_usage > 0.9",
      evaluation_window_seconds: 30,
      consecutive_breaches: 2,
      severity: "warning"
    }
  ]
  
  // 注册故障检测规则
  for rule in detection_rules {
    azimuth::FaultDetector::register_rule(fault_detector, rule)
  }
  
  // 模拟健康指标
  let health_metrics = [
    { timestamp: azimuth::Clock::now_unix_nanos(azimuth::Clock::system()), error_rate: 0.05, p95_latency: 200, availability: 0.99, memory_usage: 0.6 },
    { timestamp: azimuth::Clock::now_unix_nanos(azimuth::Clock::system()) + 10000000000L, error_rate: 0.08, p95_latency: 300, availability: 0.98, memory_usage: 0.7 },
    { timestamp: azimuth::Clock::now_unix_nanos(azimuth::Clock::system()) + 20000000000L, error_rate: 0.12, p95_latency: 500, availability: 0.97, memory_usage: 0.8 },
    { timestamp: azimuth::Clock::now_unix_nanos(azimuth::Clock::system()) + 30000000000L, error_rate: 0.15, p95_latency: 1200, availability: 0.94, memory_usage: 0.92 }
  ]
  
  // 提交健康指标进行检测
  let detected_faults = []
  
  for metric in health_metrics {
    let faults = azimuth::FaultDetector::evaluate_metrics(fault_detector, metric)
    for fault in faults {
      detected_faults.push(fault)
    }
  }
  
  // 验证故障检测
  assert_true(detected_faults.length >= 2)  # 至少检测到2个故障
  
  # 应该检测到高错误率
  let error_rate_faults = detected_faults.filter(fn(f) { f.rule_name == "high_error_rate" })
  assert_true(error_rate_faults.length >= 1)
  
  # 应该检测到高延迟
  let latency_faults = detected_faults.filter(fn(f) { f.rule_name == "high_latency" })
  assert_true(latency_faults.length >= 1)
  
  # 应该检测到服务不可用
  let availability_faults = detected_faults.filter(fn(f) { f.rule_name == "service_unavailable" })
  assert_true(availability_faults.length >= 1)
  
  # 应该检测到内存压力
  let memory_faults = detected_faults.filter(fn(f) { f.rule_name == "memory_pressure" })
  assert_true(memory_faults.length >= 1)
  
  // 测试故障诊断
  let diagnostic_results = []
  
  for fault in detected_faults {
    let diagnosis = azimuth::FaultDetector::diagnose_fault(fault_detector, fault)
    diagnostic_results.push(diagnosis)
  }
  
  // 验证诊断结果
  for diagnosis in diagnostic_results {
    assert_true(diagnosis.possible_causes.length >= 1)
    assert_true(diagnosis.recommended_actions.length >= 1)
    assert_true(diagnosis.confidence_score >= 0.0 && diagnosis.confidence_score <= 1.0)
  }
  
  // 测试故障预测
  let prediction_window_seconds = 300  # 5分钟预测窗口
  let fault_predictions = azimuth::FaultDetector::predict_potential_faults(fault_detector, prediction_window_seconds)
  
  assert_true(fault_predictions.length >= 0)
  
  for prediction in fault_predictions {
    assert_true(prediction.probability >= 0.0 && prediction.probability <= 1.0)
    assert_true(prediction.time_to_fault_seconds > 0)
    assert_true(prediction.predicted_fault_type != "")
  }
  
  // 测试故障相关性分析
  let correlation_analysis = azimuth::FaultDetector::analyze_fault_correlations(fault_detector, detected_faults)
  
  assert_true(correlation_analysis.correlated_groups.length >= 0)
  
  for group in correlation_analysis.correlated_groups {
    assert_true(group.faults.length >= 2)
    assert_true(group.correlation_strength >= 0.0 && group.correlation_strength <= 1.0)
    assert_true(group.common_causes.length >= 1)
  }
  
  // 测试故障影响评估
  let impact_assessment = azimuth::FaultDetector::assess_fault_impact(fault_detector, detected_faults)
  
  assert_true(impact_assessment.overall_impact_score >= 0.0 && impact_assessment.overall_impact_score <= 1.0)
  assert_true(impact_assessment.affected_components.length >= 1)
  assert_true(impact_assessment.business_impact_description != "")
  
  // 验证影响评估的合理性
  let critical_faults = detected_faults.filter(fn(f) { f.severity == "critical" })
  if critical_faults.length > 0 {
    assert_true(impact_assessment.overall_impact_score > 0.5)
  }
}

pub test "自动恢复机制测试" {
  let resilience_manager = azimuth::ResilienceManager::new()
  
  // 创建自动恢复管理器
  let recovery_manager = azimuth::ResilienceManager::create_recovery_manager(resilience_manager)
  
  // 配置恢复策略
  let recovery_strategies = [
    {
      fault_type: "high_error_rate",
      recovery_actions: [
        { action: "restart_service", delay_seconds: 0, max_attempts: 3 },
        { action: "enable_circuit_breaker", delay_seconds: 10, max_attempts: 1 },
        { action: "scale_up", delay_seconds: 30, max_attempts: 2 }
      ]
    },
    {
      fault_type: "high_latency",
      recovery_actions: [
        { action: "clear_cache", delay_seconds: 0, max_attempts: 3 },
        { action: "optimize_thread_pool", delay_seconds: 5, max_attempts: 1 },
        { action: "increase_memory", delay_seconds: 15, max_attempts: 2 }
      ]
    },
    {
      fault_type: "service_unavailable",
      recovery_actions: [
        { action: "restart_service", delay_seconds: 0, max_attempts: 5 },
        { action: "failover_to_backup", delay_seconds: 10, max_attempts: 1 },
        { action: "enable_degraded_mode", delay_seconds: 20, max_attempts: 1 }
      ]
    },
    {
      fault_type: "memory_pressure",
      recovery_actions: [
        { action: "garbage_collect", delay_seconds: 0, max_attempts: 5 },
        { action: "clear_buffers", delay_seconds: 5, max_attempts: 3 },
        { action: "restart_service", delay_seconds: 30, max_attempts: 2 }
      ]
    }
  ]
  
  // 注册恢复策略
  for strategy in recovery_strategies {
    azimuth::RecoveryManager::register_strategy(recovery_manager, strategy)
  }
  
  // 模拟故障场景
  let fault_scenarios = [
    {
      fault_type: "high_error_rate",
      severity: "critical",
      affected_component: "telemetry-collector",
      metrics: { error_rate: 0.15, availability: 0.85 }
    },
    {
      fault_type: "high_latency",
      severity: "warning",
      affected_component: "metrics-aggregator",
      metrics: { p95_latency: 1500, throughput: 800 }
    },
    {
      fault_type: "service_unavailable",
      severity: "critical",
      affected_component: "trace-exporter",
      metrics: { availability: 0.0, connection_errors: 100 }
    },
    {
      fault_type: "memory_pressure",
      severity: "warning",
      affected_component: "log-processor",
      metrics: { memory_usage: 0.93, gc_frequency: 10 }
    }
  ]
  
  // 测试自动恢复
  let recovery_results = []
  
  for scenario in fault_scenarios {
    let recovery_start = azimuth::Clock::now_unix_nanos(azimuth::Clock::system())
    
    let recovery_result = azimuth::RecoveryManager::initiate_recovery(recovery_manager, scenario)
    
    let recovery_end = azimuth::Clock::now_unix_nanos(azimuth::Clock::system())
    let recovery_duration = (recovery_end - recovery_start) / 1000000L
    
    recovery_results.push({
      scenario: scenario.fault_type,
      success: recovery_result.success,
      actions_taken: recovery_result.actions_taken,
      recovery_time_ms: recovery_duration,
      final_state: recovery_result.final_state
    })
  }
  
  // 验证恢复结果
  for result in recovery_results {
    assert_true(result.success || result.actions_taken.length > 0)  # 要么成功，要么尝试了恢复动作
    assert_true(result.recovery_time_ms < 60000)  # 恢复时间应该在1分钟内
    assert_true(result.final_state != "")
  }
  
  // 测试恢复动作执行详情
  let detailed_results = []
  
  for scenario in fault_scenarios {
    let detailed_result = azimuth::RecoveryManager::execute_recovery_with_details(recovery_manager, scenario)
    detailed_results.push(detailed_result)
  }
  
  // 验证详细执行结果
  for detailed in detailed_results {
    assert_true(detailed.action_results.length >= 1)
    
    for action_result in detailed.action_results {
      assert_true(action_result.action_name != "")
      assert_true(action_result.execution_time_ms >= 0)
      assert_true(action_result.success || action_result.error_message != "")
    }
  }
  
  // 测试恢复策略优化
  let optimization_result = azimuth::RecoveryManager::optimize_recovery_strategies(recovery_manager, {
    optimization_window_hours: 24,
    success_rate_threshold: 0.8,
    time_threshold_ms: 30000
  })
  
  assert_true(optimization_result.optimized_strategies.length >= 0)
  
  for optimized in optimization_result.optimized_strategies {
    assert_true(optimized.original_success_rate <= optimized.optimized_success_rate)
    assert_true(optimized.original_avg_time_ms >= optimized.optimized_avg_time_ms)
  }
  
  // 测试恢复回滚机制
  let rollback_test_scenario = {
    fault_type: "high_error_rate",
    severity: "critical",
    affected_component: "test-component",
    metrics: { error_rate: 0.2, availability: 0.7 }
  }
  
  # 配置一个会失败的恢复动作用于测试回滚
  azimuth::RecoveryManager::add_failing_action(recovery_manager, "restart_service")
  
  let rollback_result = azimuth::RecoveryManager::initiate_recovery_with_rollback(recovery_manager, rollback_test_scenario)
  
  assert_true(rollback_result.rollback_initiated)
  assert_true(rollback_result.rollback_success || rollback_result.rollback_error_message != "")
  
  // 测试恢复状态跟踪
  let recovery_state = azimuth::RecoveryManager::get_recovery_state(recovery_manager)
  
  assert_true(recovery_state.active_recoveries >= 0)
  assert_true(recovery_state.completed_recoveries >= fault_scenarios.length())
  assert_true(recovery_state.failed_recoveries >= 0)
  assert_true(recovery_state.average_recovery_time_ms >= 0)
}

pub test "降级策略和优雅降级测试" {
  let resilience_manager = azimuth::ResilienceManager::new()
  
  // 创建降级管理器
  let degradation_manager = azimuth::ResilienceManager::create_degradation_manager(resilience_manager)
  
  // 配置降级策略
  let degradation_strategies = [
    {
      name: "trace_sampling_degradation",
      trigger_conditions: {
        error_rate: 0.1,
        latency_p95_ms: 1000,
        memory_usage: 0.8
      },
      degradation_levels: [
        {
          level: 1,
          actions: [
            { action: "reduce_sampling_rate", parameters: { new_rate: 0.5 } },
            { action: "disable_detailed_attributes", parameters: {} }
          ]
        },
        {
          level: 2,
          actions: [
            { action: "reduce_sampling_rate", parameters: { new_rate: 0.1 } },
            { action: "disable_span_events", parameters: {} },
            { action: "batch_exports", parameters: { batch_size: 1000 } }
          ]
        },
        {
          level: 3,
          actions: [
            { action: "disable_tracing", parameters: {} },
            { action: "keep_metrics_only", parameters: {} }
          ]
        }
      ]
    },
    {
      name: "metrics_degradation",
      trigger_conditions: {
        error_rate: 0.05,
        latency_p95_ms: 500,
        cpu_usage: 0.9
      },
      degradation_levels: [
        {
          level: 1,
          actions: [
            { action: "reduce_metric_frequency", parameters: { interval_seconds: 30 } },
            { action: "disable_high_cardinality_metrics", parameters: {} }
          ]
        },
        {
          level: 2,
          actions: [
            { action: "aggregate_metrics_locally", parameters: { window_seconds: 60 } },
            { action: "reduce_metric_precision", parameters: { precision: 2 } }
          ]
        },
        {
          level: 3,
          actions: [
            { action: "disable_detailed_metrics", parameters: {} },
            { action: "keep_basic_metrics_only", parameters: {} }
          ]
        }
      ]
    },
    {
      name: "logging_degradation",
      trigger_conditions: {
        error_rate: 0.15,
        disk_usage: 0.9,
        throughput: 1000
      },
      degradation_levels: [
        {
          level: 1,
          actions: [
            { action: "increase_log_level", parameters: { new_level: "WARN" } },
            { action: "reduce_log_detail", parameters: {} }
          ]
        },
        {
          level: 2,
          actions: [
            { action: "enable_log_sampling", parameters: { sampling_rate: 0.1 } },
            { action: "compress_logs", parameters: { compression: "gzip" } }
          ]
        },
        {
          level: 3,
          actions: [
            { action: "disable_debug_logs", parameters: {} },
            { action: "emergency_log_only", parameters: { level: "ERROR" } }
          ]
        }
      ]
    }
  ]
  
  // 注册降级策略
  for strategy in degradation_strategies {
    azimuth::DegradationManager::register_strategy(degradation_manager, strategy)
  }
  
  // 测试降级触发
  let degradation_scenarios = [
    {
      strategy_name: "trace_sampling_degradation",
      current_metrics: {
        error_rate: 0.12,
        latency_p95_ms: 1200,
        memory_usage: 0.85
      },
      expected_level: 2
    },
    {
      strategy_name: "metrics_degradation",
      current_metrics: {
        error_rate: 0.08,
        latency_p95_ms: 600,
        cpu_usage: 0.95
      },
      expected_level: 2
    },
    {
      strategy_name: "logging_degradation",
      current_metrics: {
        error_rate: 0.2,
        disk_usage: 0.95,
        throughput: 1500
      },
      expected_level: 3
    }
  ]
  
  let degradation_results = []
  
  for scenario in degradation_scenarios {
    let degradation_result = azimuth::DegradationManager::evaluate_and_apply(degradation_manager, scenario)
    degradation_results.push(degradation_result)
  }
  
  // 验证降级结果
  for result in degradation_results {
    assert_true(result.degradation_triggered)
    assert_true(result.applied_level >= 1)
    assert_true(result.applied_actions.length >= 1)
    
    # 验证应用的动作符合预期级别
    for action in result.applied_actions {
      assert_true(action.success || action.error_message != "")
    }
  }
  
  // 测试降级级别递增
  let progressive_degradation_scenario = {
    strategy_name: "trace_sampling_degradation",
    current_metrics: {
      error_rate: 0.05,  # 开始时正常
      latency_p95_ms: 400,
      memory_usage: 0.6
    }
  }
  
  let progressive_results = []
  
  # 模拟系统状态逐渐恶化
  let worsening_metrics = [
    { error_rate: 0.08, latency_p95_ms: 600, memory_usage: 0.7 },
    { error_rate: 0.12, latency_p95_ms: 900, memory_usage: 0.8 },
    { error_rate: 0.18, latency_p95_ms: 1400, memory_usage: 0.9 }
  ]
  
  for metrics in worsening_metrics {
    progressive_degradation_scenario.current_metrics = metrics
    
    let result = azimuth::DegradationManager::evaluate_and_apply(degradation_manager, progressive_degradation_scenario)
    progressive_results.push(result)
  }
  
  # 验证降级级别递增
  for i in 1..progressive_results.length() {
    assert_true(progressive_results[i].applied_level >= progressive_results[i-1].applied_level)
  }
  
  // 测试降级恢复
  let recovery_scenario = {
    strategy_name: "trace_sampling_degradation",
    current_metrics: {
      error_rate: 0.03,  # 恢复正常
      latency_p95_ms: 200,
      memory_usage: 0.5
    }
  }
  
  # 先触发降级
  let initial_degradation = azimuth::DegradationManager::evaluate_and_apply(degradation_manager, {
    strategy_name: "trace_sampling_degradation",
    current_metrics: {
      error_rate: 0.15,
      latency_p95_ms: 1200,
      memory_usage: 0.85
    }
  })
  
  assert_true(initial_degradation.degradation_triggered)
  
  # 等待一段时间后尝试恢复
  azimuth::Clock::sleep(5000)  # 5秒
  
  let recovery_result = azimuth::DegradationManager::attempt_recovery(degradation_manager, recovery_scenario)
  
  assert_true(recovery_result.recovery_initiated)
  assert_true(recovery_result.recovery_success || recovery_result.recovery_error_message != "")
  
  if recovery_result.recovery_success {
    assert_true(recovery_result.restored_actions.length >= 1)
  }
  
  // 测试降级性能影响
  let performance_impact_scenarios = [
    {
      strategy_name: "trace_sampling_degradation",
      degradation_level: 1,
      expected_impact: { throughput_change: -0.2, latency_change: 0.1, memory_change: -0.3 }
    },
    {
      strategy_name: "trace_sampling_degradation",
      degradation_level: 2,
      expected_impact: { throughput_change: -0.5, latency_change: 0.2, memory_change: -0.6 }
    },
    {
      strategy_name: "trace_sampling_degradation",
      degradation_level: 3,
      expected_impact: { throughput_change: -0.8, latency_change: 0.3, memory_change: -0.8 }
    }
  ]
  
  let impact_results = []
  
  for scenario in performance_impact_scenarios {
    let impact_result = azimuth::DegradationManager::measure_performance_impact(degradation_manager, scenario)
    impact_results.push(impact_result)
  }
  
  // 验证性能影响在预期范围内
  for result in impact_results {
    assert_true(result.actual_impact.throughput_change >= result.expected_impact.throughput_change - 0.1)
    assert_true(result.actual_impact.throughput_change <= result.expected_impact.throughput_change + 0.1)
    
    assert_true(result.actual_impact.memory_change <= result.expected_impact.memory_change + 0.1)
  }
  
  // 测试降级策略自动优化
  let optimization_result = azimuth::DegradationManager::optimize_strategies(degradation_manager, {
    analysis_period_hours: 24,
    performance_threshold: 0.8,
    recovery_rate_threshold: 0.9
  })
  
  assert_true(optimization_result.optimized_strategies.length >= 0)
  
  for optimized in optimization_result.optimized_strategies {
    assert_true(optimized.improvement_score >= 0.0)
    assert_true(optimized.recommended_changes.length >= 1)
  }
}

pub test "系统弹性和混沌工程测试" {
  let resilience_manager = azimuth::ResilienceManager::new()
  
  // 创建混沌工程管理器
  let chaos_manager = azimuth::ResilienceManager::create_chaos_manager(resilience_manager)
  
  // 配置混沌实验
  let chaos_experiments = [
    {
      name: "network_latency_injection",
      type: "network",
      parameters: {
        latency_ms: 500,
        jitter_ms: 100,
        packet_loss_percentage: 5,
        affected_services: ["telemetry-collector", "metrics-aggregator"]
      },
      duration_seconds: 60,
      blast_radius: "medium"
    },
    {
      name: "cpu_stress_injection",
      type: "resource",
      parameters: {
        cpu_load_percentage: 80,
        duration_seconds: 45,
        target_processes: ["otel-collector", "metrics-processor"]
      },
      duration_seconds: 45,
      blast_radius: "low"
    },
    {
      name: "memory_pressure_injection",
      type: "resource",
      parameters: {
        memory_consumption_mb: 512,
        allocation_rate_mb_per_sec: 50,
        target_processes: ["trace-exporter"]
      },
      duration_seconds: 30,
      blast_radius: "low"
    },
    {
      name: "service_kill_injection",
      type: "process",
      parameters: {
        kill_signal: "SIGTERM",
        target_services: ["log-processor"],
        restart_delay_seconds: 10
      },
      duration_seconds: 20,
      blast_radius: "high"
    }
  ]
  
  // 注册混沌实验
  for experiment in chaos_experiments {
    azimuth::ChaosManager::register_experiment(chaos_manager, experiment)
  }
  
  // 测试混沌实验执行
  let experiment_results = []
  
  for experiment in chaos_experiments {
    # 记录基线指标
    let baseline_metrics = azimuth::ChaosManager::collect_baseline_metrics(chaos_manager, experiment)
    
    # 执行混沌实验
    let execution_start = azimuth::Clock::now_unix_nanos(azimuth::Clock::system())
    
    let execution_result = azimuth::ChaosManager::execute_experiment(chaos_manager, experiment)
    
    let execution_end = azimuth::Clock::now_unix_nanos(azimuth::Clock::system())
    let execution_duration = (execution_end - execution_start) / 1000000L
    
    # 收集实验期间的指标
    let experiment_metrics = azimuth::ChaosManager::collect_experiment_metrics(chaos_manager, experiment)
    
    # 分析弹性指标
    let resilience_analysis = azimuth::ChaosManager::analyze_resilience(chaos_manager, {
      baseline: baseline_metrics,
      experiment: experiment_metrics,
      experiment_config: experiment
    })
    
    experiment_results.push({
      experiment_name: experiment.name,
      execution_success: execution_result.success,
      execution_duration_ms: execution_duration,
      resilience_score: resilience_analysis.overall_score,
      availability_impact: resilience_analysis.availability_impact,
      performance_impact: resilience_analysis.performance_impact,
      recovery_time_seconds: resilience_analysis.recovery_time_seconds
    })
  }
  
  // 验证混沌实验结果
  for result in experiment_results {
    assert_true(result.execution_success)
    assert_true(result.resilience_score >= 0.0 && result.resilience_score <= 1.0)
    assert_true(result.availability_impact >= 0.0 && result.availability_impact <= 1.0)
    assert_true(result.performance_impact >= -1.0 && result.performance_impact <= 0.0)
    assert_true(result.recovery_time_seconds >= 0)
  }
  
  // 测试系统弹性基准
  let resilience_benchmark = azimuth::ChaosManager::run_resilience_benchmark(chaos_manager, {
    test_scenarios: [
      "network_partition",
      "resource_exhaustion",
      "process_crash",
      "dependency_failure"
    ],
    measurement_duration_seconds: 120,
    recovery_timeout_seconds: 60
  })
  
  assert_true(resilience_benchmark.overall_score >= 0.0 && resilience_benchmark.overall_score <= 1.0)
  assert_true(resilience_benchmark.scenario_results.length >= 4)
  
  for scenario_result in resilience_benchmark.scenario_results {
    assert_true(scenario_result.resilience_score >= 0.0 && scenario_result.resilience_score <= 1.0)
    assert_true(scenario_result.mttf_seconds >= 0)  # Mean Time To Failure
    assert_true(scenario_result.mttr_seconds >= 0)  # Mean Time To Recovery
  }
  
  // 测试故障注入的安全机制
  let safety_checks = [
    {
      check_name: "critical_service_protection",
      condition: "service.name == 'authentication-service'",
      action: "prevent_experiment"
    },
    {
      check_name: "business_hours_protection",
      condition: "current_time in business_hours",
      action: "require_approval"
    },
    {
      check_name: "load_threshold_protection",
      condition: "system.load > 0.8",
      action: "prevent_experiment"
    }
  ]
  
  for check in safety_checks {
    azimuth::ChaosManager::add_safety_check(chaos_manager, check)
  }
  
  # 测试安全检查
  let dangerous_experiment = {
    name: "dangerous_experiment",
    type: "network",
    target_services: ["authentication-service"],
    parameters: {}
  }
  
  let safety_result = azimuth::ChaosManager::validate_experiment_safety(chaos_manager, dangerous_experiment)
  assert_false(safety_result.allowed)  # 应该被安全检查阻止
  assert_true(safety_result.blocked_by.length >= 1)
  
  // 测试渐进式故障注入
  let progressive_experiment = {
    name: "progressive_latency_injection",
    type: "network",
    parameters: {
      initial_latency_ms: 50,
      max_latency_ms: 1000,
      increment_ms: 50,
      increment_interval_seconds: 10,
      target_services: ["metrics-aggregator"]
    },
    duration_seconds: 120,
    blast_radius: "medium"
  }
  
  let progressive_result = azimuth::ChaosManager::execute_progressive_experiment(chaos_manager, progressive_experiment)
  
  assert_true(progressive_result.success)
  assert_true(progressive_result.stages_completed >= 1)
  assert_true(progressive_result.max_latency_reached <= progressive_experiment.parameters.max_latency_ms)
  
  // 测试弹性改进建议
  let improvement_recommendations = azimuth::ChaosManager::generate_improvement_recommendations(chaos_manager, {
    analysis_period_hours: 24,
    focus_areas: ["availability", "performance", "recovery"],
    min_resilience_score: 0.7
  })
  
  assert_true(improvement_recommendations.recommendations.length >= 0)
  
  for recommendation in improvement_recommendations.recommendations {
    assert_true(recommendation.category != "")
    assert_true(recommendation.description != "")
    assert_true(recommendation.priority >= 1 && recommendation.priority <= 5)
    assert_true(recommendation.estimated_impact >= 0.0 && recommendation.estimated_impact <= 1.0)
  }
  
  // 测试弹性报告生成
  let resilience_report = azimuth::ChaosManager::generate_resilience_report(chaos_manager, {
    report_period_hours: 24,
    include_recommendations: true,
    include_trends: true,
    format: "detailed"
  })
  
  assert_true(resilience_report.generated_at > 0)
  assert_true(resilience_report.overall_resilience_score >= 0.0 && resilience_report.overall_resilience_score <= 1.0)
  assert_true(resilience_report.experiment_summary.total_experiments >= experiment_results.length())
  assert_true(resilience_report.trend_analysis.resilience_trend != "")
  
  // 验证报告包含关键信息
  assert_true(resilience_report.key_findings.length >= 1)
  assert_true(resilience_report.weaknesses_identified.length >= 0)
  assert_true(resilience_report.strengths_identified.length >= 1)
}