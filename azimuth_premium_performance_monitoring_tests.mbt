// Azimuth Premium Performance Monitoring and Alerting Test Suite
// 高级性能监控和智能告警测试套件 - 专注于实时性能监控和智能告警系统

// Test 1: 实时性能指标监控
test "real-time performance metrics monitoring" {
  // 创建实时性能监控器
  let performance_monitor = @azimuth.RealTimePerformanceMonitor::new(1000) // 1秒采样间隔
  
  // 配置监控指标
  let monitored_metrics = [
    @azimuth.MetricDefinition::new("cpu.usage", @azimuth.MetricType::Gauge, @azimuth.Unit::Percentage),
    @azimuth.MetricDefinition::new("memory.usage", @azimuth.MetricType::Gauge, @azimuth.Unit::Percentage),
    @azimuth.MetricDefinition::new("request.rate", @azimuth.MetricType::Counter, @azimuth.Unit::RequestsPerSecond),
    @azimuth.MetricDefinition::new("response.time", @azimuth.MetricType::Histogram, @azimuth.Unit::Milliseconds),
    @azimuth.MetricDefinition::new("error.rate", @azimuth.MetricType::Rate, @azimuth.Unit::Percentage)
  ]
  
  @azimuth.RealTimePerformanceMonitor::configure_metrics(performance_monitor, monitored_metrics)
  
  // 启动监控
  @azimuth.RealTimePerformanceMonitor::start(performance_monitor)
  
  // 模拟性能数据流
  let current_time = @azimuth.Time::now_unix_nanos()
  let mut metric_samples = []
  
  for i in 0..=300 { // 5分钟的数据
    let timestamp = current_time + (i * 1000000000) // 每秒一个样本
    
    let cpu_sample = @azimuth.MetricSample::new(
      "cpu.usage",
      45.0 + (i % 20) * 2.0, // 45-85%的CPU使用率
      timestamp
    )
    
    let memory_sample = @azimuth.MetricSample::new(
      "memory.usage",
      60.0 + (i % 30) * 1.0, // 60-90%的内存使用率
      timestamp
    )
    
    let request_rate_sample = @azimuth.MetricSample::new(
      "request.rate",
      100.0 + (i % 50) * 5.0, // 100-350 req/s
      timestamp
    )
    
    let response_time_sample = @azimuth.MetricSample::new(
      "response.time",
      50.0 + (i % 100) * 2.0, // 50-250ms响应时间
      timestamp
    )
    
    let error_rate_sample = @azimuth.MetricSample::new(
      "error.rate",
      if i % 20 == 0 { 5.0 } else { 0.5 }, // 偶尔5%错误率
      timestamp
    )
    
    metric_samples = metric_samples.push([cpu_sample, memory_sample, request_rate_sample, response_time_sample, error_rate_sample])
  }
  
  // 提交性能数据
  for sample_batch in metric_samples {
    for sample in sample_batch {
      @azimuth.RealTimePerformanceMonitor::record_metric(performance_monitor, sample)
    }
  }
  
  // 等待数据处理
  @azimuth.Time::sleep(100)
  
  // 获取性能统计
  let performance_stats = @azimuth.RealTimePerformanceMonitor::get_stats(performance_monitor)
  
  // 验证统计数据
  assert_true(performance_stats.monitored_metrics.length() >= 5)
  assert_true(performance_stats.total_samples >= 1500)
  assert_true(performance_stats.monitoring_duration_seconds >= 300)
  
  // 验证CPU统计
  let cpu_stats = performance_stats.metric_stats.get("cpu.usage")
  match cpu_stats {
    Some(stats) => {
      assert_true(stats.min >= 45.0)
      assert_true(stats.max <= 85.0)
      assert_true(stats.average >= 60.0)
      assert_true(stats.average <= 70.0)
      assert_true(stats.sample_count >= 300)
    }
    None => assert_true(false)
  }
  
  // 验证内存统计
  let memory_stats = performance_stats.metric_stats.get("memory.usage")
  match memory_stats {
    Some(stats) => {
      assert_true(stats.min >= 60.0)
      assert_true(stats.max <= 90.0)
      assert_true(stats.average >= 70.0)
      assert_true(stats.average <= 80.0)
    }
    None => assert_true(false)
  }
  
  // 验证响应时间统计
  let response_time_stats = performance_stats.metric_stats.get("response.time")
  match response_time_stats {
    Some(stats) => {
      assert_true(stats.min >= 50.0)
      assert_true(stats.max <= 250.0)
      assert_true(stats.p95 >= 150.0)
      assert_true(stats.p99 >= 200.0)
    }
    None => assert_true(false)
  }
  
  // 停止监控
  @azimuth.RealTimePerformanceMonitor::stop(performance_monitor)
}

// Test 2: 智能告警系统
test "intelligent alerting system" {
  // 创建智能告警系统
  let alerting_system = @azimuth.IntelligentAlertingSystem::new()
  
  // 配置告警规则
  let alert_rules = [
    @azimuth.AlertRule::new(
      "high_cpu_usage",
      "CPU使用率过高",
      @azimuth.Condition::GreaterThan("cpu.usage", 80.0),
      @azimuth.Severity::Warning,
      @azimuth.Duration::Seconds(60), // 持续60秒
      @azimuth.NotificationChannel::Email(["ops@example.com"])
    ),
    @azimuth.AlertRule::new(
      "critical_memory_usage",
      "内存使用率严重过高",
      @azimuth.Condition::GreaterThan("memory.usage", 90.0),
      @azimuth.Severity::Critical,
      @azimuth.Duration::Seconds(30), // 持续30秒
      @azimuth.NotificationChannel::SMS(["+1234567890"])
    ),
    @azimuth.AlertRule::new(
      "high_error_rate",
      "错误率过高",
      @azimuth.Condition::GreaterThan("error.rate", 5.0),
      @azimuth.Severity::Error,
      @azimuth.Duration::Seconds(120), // 持续2分钟
      @azimuth.NotificationChannel::Slack(["#alerts"])
    ),
    @azimuth.AlertRule::new(
      "slow_response_time",
      "响应时间过慢",
      @azimuth.Condition::GreaterThan("response.time.p95", 200.0),
      @azimuth.Severity::Warning,
      @azimuth.Duration::Seconds(300), // 持续5分钟
      @azimuth.NotificationChannel::Webhook(["https://api.example.com/alerts"])
    )
  ]
  
  @azimuth.IntelligentAlertingSystem::configure_rules(alerting_system, alert_rules)
  
  // 启动告警系统
  @azimuth.IntelligentAlertingSystem::start(alerting_system)
  
  // 模拟触发告警的场景
  let current_time = @azimuth.Time::now_unix_nanos()
  
  // 场景1: CPU使用率持续过高（触发警告）
  for i in 0..=120 { // 2分钟的高CPU使用率
    let cpu_metric = @azimuth.MetricSample::new("cpu.usage", 85.0, current_time + (i * 1000000000))
    @azimuth.IntelligentAlertingSystem::process_metric(alerting_system, cpu_metric)
  }
  
  // 场景2: 内存使用率严重过高（触发严重告警）
  for i in 0..=60 { // 1分钟的严重内存使用率
    let memory_metric = @azimuth.MetricSample::new("memory.usage", 95.0, current_time + (i * 1000000000))
    @azimuth.IntelligentAlertingSystem::process_metric(alerting_system, memory_metric)
  }
  
  // 场景3: 错误率突然升高（触发错误告警）
  for i in 0..=180 { // 3分钟的高错误率
    let error_metric = @azimuth.MetricSample::new("error.rate", 8.0, current_time + (i * 1000000000))
    @azimuth.IntelligentAlertingSystem::process_metric(alerting_system, error_metric)
  }
  
  // 等待告警处理
  @azimuth.Time::sleep(500)
  
  // 获取告警状态
  let alert_status = @azimuth.IntelligentAlertingSystem::get_alert_status(alerting_system)
  
  // 验证告警触发
  assert_true(alert_status.active_alerts.length() >= 2)
  assert_true(alert_status.triggered_alerts.length() >= 2)
  
  // 验证特定告警
  let cpu_alert = alert_status.active_alerts.find(fn(alert) { alert.rule_id == "high_cpu_usage" })
  match cpu_alert {
    Some(alert) => {
      assert_eq(alert.severity, @azimuth.Severity::Warning)
      assert_true(alert.triggered_at > 0)
      assert_true(alert.duration_seconds >= 60)
    }
    None => assert_true(false)
  }
  
  let memory_alert = alert_status.active_alerts.find(fn(alert) { alert.rule_id == "critical_memory_usage" })
  match memory_alert {
    Some(alert) => {
      assert_eq(alert.severity, @azimuth.Severity::Critical)
      assert_true(alert.triggered_at > 0)
      assert_true(alert.duration_seconds >= 30)
    }
    None => assert_true(false)
  }
  
  let error_alert = alert_status.active_alerts.find(fn(alert) { alert.rule_id == "high_error_rate" })
  match error_alert {
    Some(alert) => {
      assert_eq(alert.severity, @azimuth.Severity::Error)
      assert_true(alert.triggered_at > 0)
      assert_true(alert.duration_seconds >= 120)
    }
    None => assert_true(false)
  }
  
  // 测试告警恢复
  // 模拟指标恢复正常
  for i in 0..=60 {
    let cpu_metric = @azimuth.MetricSample::new("cpu.usage", 45.0, current_time + (i * 1000000000))
    let memory_metric = @azimuth.MetricSample::new("memory.usage", 65.0, current_time + (i * 1000000000))
    let error_metric = @azimuth.MetricSample::new("error.rate", 0.5, current_time + (i * 1000000000))
    
    @azimuth.IntelligentAlertingSystem::process_metric(alerting_system, cpu_metric)
    @azimuth.IntelligentAlertingSystem::process_metric(alerting_system, memory_metric)
    @azimuth.IntelligentAlertingSystem::process_metric(alerting_system, error_metric)
  }
  
  // 等待告警恢复处理
  @azimuth.Time::sleep(200)
  
  // 验证告警恢复
  let recovered_alert_status = @azimuth.IntelligentAlertingSystem::get_alert_status(alerting_system)
  assert_true(recovered_alert_status.resolved_alerts.length() >= 2)
  
  // 停止告警系统
  @azimuth.IntelligentAlertingSystem::stop(alerting_system)
}

// Test 3: 自适应阈值调整
test "adaptive threshold adjustment" {
  // 创建自适应阈值管理器
  let adaptive_threshold_manager = @azimuth.AdaptiveThresholdManager::new()
  
  // 配置自适应阈值策略
  let threshold_strategies = [
    @azimuth.ThresholdStrategy::new(
      "cpu.usage",
      @azimuth.AdaptationMethod::Statistical,
      @azimuth.AdaptationConfig::new(0.95, 2.0, 3600) // 95%置信度，2倍标准差，1小时调整周期
    ),
    @azimuth.ThresholdStrategy::new(
      "memory.usage",
      @azimuth.AdaptationMethod::MachineLearning,
      @azimuth.AdaptationConfig::new(0.9, 1.5, 1800) // 90%置信度，1.5倍标准差，30分钟调整周期
    ),
    @azimuth.ThresholdStrategy::new(
      "response.time",
      @azimuth.AdaptationMethod::TimeSeriesAnalysis,
      @azimuth.AdaptationConfig::new(0.85, 2.5, 7200) // 85%置信度，2.5倍标准差，2小时调整周期
    )
  ]
  
  @azimuth.AdaptiveThresholdManager::configure_strategies(adaptive_threshold_manager, threshold_strategies)
  
  // 启动自适应调整
  @azimuth.AdaptiveThresholdManager::start(adaptive_threshold_manager)
  
  // 模拟历史数据（用于训练自适应模型）
  let current_time = @azimuth.Time::now_unix_nanos()
  let mut historical_data = []
  
  for i in 0..=1440 { // 24小时的历史数据
    let timestamp = current_time - ((1440 - i) * 60000000000) // 从24小时前开始
    
    // 模拟日周期性模式
    let hour_of_day = (timestamp / 3600000000000) % 24
    let base_cpu = 40.0 + (hour_of_day / 24.0) * 30.0 // 40-70%基础CPU
    let base_memory = 50.0 + (hour_of_day / 24.0) * 20.0 // 50-70%基础内存
    let base_response_time = 80.0 + (hour_of_day / 24.0) * 40.0 // 80-120ms基础响应时间
    
    // 添加随机噪声
    let cpu_value = base_cpu + (@azimuth.Math::random() * 10.0 - 5.0)
    let memory_value = base_memory + (@azimuth.Math::random() * 8.0 - 4.0)
    let response_time_value = base_response_time + (@azimuth.Math::random() * 20.0 - 10.0)
    
    historical_data = historical_data.push([
      @azimuth.MetricSample::new("cpu.usage", cpu_value, timestamp),
      @azimuth.MetricSample::new("memory.usage", memory_value, timestamp),
      @azimuth.MetricSample::new("response.time", response_time_value, timestamp)
    ])
  }
  
  // 提交历史数据
  for data_batch in historical_data {
    for sample in data_batch {
      @azimuth.AdaptiveThresholdManager::process_historical_data(adaptive_threshold_manager, sample)
    }
  }
  
  // 触发阈值计算
  @azimuth.AdaptiveThresholdManager::calculate_adaptive_thresholds(adaptive_threshold_manager)
  
  // 获取自适应阈值
  let adaptive_thresholds = @azimuth.AdaptiveThresholdManager::get_adaptive_thresholds(adaptive_threshold_manager)
  
  // 验证自适应阈值
  assert_true(adaptive_thresholds.length() >= 3)
  
  let cpu_threshold = adaptive_thresholds.get("cpu.usage")
  match cpu_threshold {
    Some(threshold) => {
      assert_true(threshold.upper_bound > 70.0)
      assert_true(threshold.upper_bound < 90.0)
      assert_true(threshold.confidence_level >= 0.9)
      assert_true(threshold.last_updated > 0)
    }
    None => assert_true(false)
  }
  
  let memory_threshold = adaptive_thresholds.get("memory.usage")
  match memory_threshold {
    Some(threshold) => {
      assert_true(threshold.upper_bound > 70.0)
      assert_true(threshold.upper_bound < 85.0)
      assert_true(threshold.confidence_level >= 0.85)
    }
    None => assert_true(false)
  }
  
  let response_time_threshold = adaptive_thresholds.get("response.time")
  match response_time_threshold {
    Some(threshold) => {
      assert_true(threshold.upper_bound > 120.0)
      assert_true(threshold.upper_bound < 180.0)
      assert_true(threshold.confidence_level >= 0.8)
    }
    None => assert_true(false)
  }
  
  // 测试阈值的动态调整
  // 模拟新的负载模式（更高的CPU使用率）
  let new_pattern_start = current_time
  for i in 0..=120 {
    let timestamp = new_pattern_start + (i * 60000000000) // 每分钟一个样本
    let elevated_cpu = 75.0 + (@azimuth.Math::random() * 15.0) // 75-90% CPU使用率
    let cpu_sample = @azimuth.MetricSample::new("cpu.usage", elevated_cpu, timestamp)
    @azimuth.AdaptiveThresholdManager::process_realtime_data(adaptive_threshold_manager, cpu_sample)
  }
  
  // 触发阈值重新计算
  @azimuth.AdaptiveThresholdManager::calculate_adaptive_thresholds(adaptive_threshold_manager)
  
  // 验证阈值调整
  let updated_thresholds = @azimuth.AdaptiveThresholdManager::get_adaptive_thresholds(adaptive_threshold_manager)
  let updated_cpu_threshold = updated_thresholds.get("cpu.usage")
  
  match updated_cpu_threshold {
    Some(threshold) => {
      // 阈值应该已经向上调整
      assert_true(threshold.upper_bound > cpu_threshold.unwrap().upper_bound)
      assert_true(threshold.last_updated > cpu_threshold.unwrap().last_updated)
    }
    None => assert_true(false)
  }
  
  // 停止自适应阈值管理器
  @azimuth.AdaptiveThresholdManager::stop(adaptive_threshold_manager)
}

// Test 4: 告警聚合和关联分析
test "alert correlation and aggregation analysis" {
  // 创建告警关联分析器
  let alert_correlator = @azimuth.AlertCorrelator::new()
  
  // 配置关联规则
  let correlation_rules = [
    @azimuth.CorrelationRule::new(
      "performance_degradation",
      "性能降级关联",
      ["high_cpu_usage", "high_memory_usage", "slow_response_time"],
      @azimuth.CorrelationType::Causal,
      @azimuth.TimeWindow::Minutes(5)
    ),
    @azimuth.CorrelationRule::new(
      "service_outage",
      "服务中断关联",
      ["high_error_rate", "service_unavailable", "database_connection_failed"],
      @azimuth.CorrelationType::Symptom,
      @azimuth.TimeWindow::Minutes(2)
    ),
    @azimuth.CorrelationRule::new(
      "resource_exhaustion",
      "资源耗尽关联",
      ["high_cpu_usage", "high_memory_usage", "disk_space_low"],
      @azimuth.CorrelationType::RootCause,
      @azimuth.TimeWindow::Minutes(10)
    )
  ]
  
  @azimuth.AlertCorrelator::configure_rules(alert_correlator, correlation_rules)
  
  // 创建测试告警序列
  let current_time = @azimuth.Time::now_unix_nanos()
  let test_alerts = [
    @azimuth.Alert::new("high_cpu_usage", @azimuth.Severity::Warning, current_time, ["service", "web"]),
    @azimuth.Alert::new("high_memory_usage", @azimuth.Severity::Warning, current_time + 30000, ["service", "web"]),
    @azimuth.Alert::new("slow_response_time", @azimuth.Severity::Error, current_time + 60000, ["service", "web"]),
    @azimuth.Alert::new("high_error_rate", @azimuth.Severity::Critical, current_time + 90000, ["service", "web"]),
    @azimuth.Alert::new("database_connection_failed", @azimuth.Severity::Critical, current_time + 120000, ["service", "database"]),
    @azimuth.Alert::new("service_unavailable", @azimuth.Severity::Critical, current_time + 150000, ["service", "web"])
  ]
  
  // 执行告警关联分析
  let correlation_result = @azimuth.AlertCorrelator::analyze_correlations(alert_correlator, test_alerts)
  
  // 验证关联结果
  assert_true(correlation_result.correlation_groups.length() >= 2)
  
  // 验证性能降级关联组
  let performance_group = correlation_result.correlation_groups.find(fn(group) {
    group.correlation_type == @azimuth.CorrelationType::Causal
  })
  match performance_group {
    Some(group) => {
      assert_true(group.alerts.length() >= 3)
      assert_true(group.confidence_score >= 0.7)
      assert_eq(group.root_cause_category, "performance_degradation")
    }
    None => assert_true(false)
  }
  
  // 验证服务中断关联组
  let outage_group = correlation_result.correlation_groups.find(fn(group) {
    group.correlation_type == @azimuth.CorrelationType::Symptom
  })
  match outage_group {
    Some(group) => {
      assert_true(group.alerts.length() >= 3)
      assert_true(group.confidence_score >= 0.8)
      assert_eq(group.root_cause_category, "service_outage")
    }
    None => assert_true(false)
  }
  
  // 测试告警聚合
  let alert_aggregator = @azimuth.AlertAggregator::new()
  let aggregation_config = @azimuth.AggregationConfig::new(
    @azimuth.AggregationStrategy::ByServiceAndSeverity,
    @azimuth.TimeWindow::Minutes(15),
    true // 启用智能聚合
  )
  
  @azimuth.AlertAggregator::configure(alert_aggregator, aggregation_config)
  
  // 执行告警聚合
  let aggregation_result = @azimuth.AlertAggregator::aggregate_alerts(alert_aggregator, test_alerts)
  
  // 验证聚合结果
  assert_true(aggregation_result.aggregated_alerts.length() < test_alerts.length())
  assert_true(aggregation_result.reduction_rate >= 0.2)
  
  // 验证聚合告警的内容
  let web_service_alerts = aggregation_result.aggregated_alerts.filter(fn(alert) {
    alert.service_tags.contains("web")
  })
  assert_true(web_service_alerts.length() >= 1)
  
  // 验证聚合告警的统计信息
  let aggregated_alert = web_service_alerts.get(0)
  match aggregated_alert {
    Some(alert) => {
      assert_true(alert.original_alert_count >= 4)
      assert_true(alert.severity >= @azimuth.Severity::Error)
      assert_true(alert.affected_services.length() >= 1)
    }
    None => assert_true(false)
  }
  
  // 测试告警抑制
  let alert_suppressor = @azimuth.AlertSuppressor::new()
  let suppression_rules = [
    @azimuth.SuppressionRule::new(
      "duplicate_suppression",
      "重复告警抑制",
      @azimuth.SuppressionCondition::SameServiceAndMetric,
      @azimuth.TimeWindow::Minutes(10)
    ),
    @azimuth.SuppressionRule::new(
      "dependency_suppression",
      "依赖告警抑制",
      @azimuth.SuppressionCondition::DependencyChain,
      @azimuth.TimeWindow::Minutes(5)
    )
  ]
  
  @azimuth.AlertSuppressor::configure_rules(alert_suppressor, suppression_rules)
  
  // 创建重复告警
  let duplicate_alerts = [
    @azimuth.Alert::new("high_cpu_usage", @azimuth.Severity::Warning, current_time, ["service", "web"]),
    @azimuth.Alert::new("high_cpu_usage", @azimuth.Severity::Warning, current_time + 120000, ["service", "web"]),
    @azimuth.Alert::new("high_cpu_usage", @azimuth.Severity::Warning, current_time + 240000, ["service", "web"])
  ]
  
  let suppression_result = @azimuth.AlertSuppressor::suppress_alerts(alert_suppressor, duplicate_alerts)
  
  // 验证抑制结果
  assert_true(suppression_result.suppressed_alerts.length() >= 2)
  assert_true(suppression_result.allowed_alerts.length() >= 1)
  assert_true(suppression_result.suppression_rate >= 0.6)
}