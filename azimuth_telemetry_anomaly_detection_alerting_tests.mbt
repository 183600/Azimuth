// Azimuth Telemetry Data Anomaly Detection and Alerting Tests
// 遥测数据异常检测和告警测试用例，专注于异常识别、告警生成和通知机制

// Test 1: 基于统计的异常检测
test "statistical-based anomaly detection" {
  // 创建异常检测器
  let detector = azimuth::anomaly::StatisticalDetector::new()
  
  // 配置统计异常检测参数
  azimuth::anomaly::set_method(detector, "z_score")
  azimuth::anomaly::set_threshold(detector, 3.0) // 3个标准差
  azimuth::anomaly::set_window_size(detector, 100) // 100个数据点的窗口
  
  // 生成正常数据
  let normal_data = []
  let mut sum = 0.0
  let mut sum_squared = 0.0
  
  for i in 0..=99 {
    // 生成均值为50，标准差为10的正态分布数据
    let value = 50.0 + azimuth::random::normal(0.0, 10.0)
    normal_data = normal_data.push(value)
    sum = sum + value
    sum_squared = sum_squared + value * value
  }
  
  // 训练检测器
  azimuth::anomaly::train(detector, normal_data)
  
  // 计算统计量
  let mean = sum / normal_data.length().to_float()
  let variance = (sum_squared / normal_data.length().to_float()) - (mean * mean)
  let std_dev = variance.sqrt()
  
  // 验证模型参数
  let model_mean = azimuth::anomaly::get_mean(detector)
  let model_std_dev = azimuth::anomaly::get_std_dev(detector)
  
  assert_true((model_mean - mean).abs() < 1.0) // 均值误差应该小于1
  assert_true((model_std_dev - std_dev).abs() < 1.0) // 标准差误差应该小于1
  
  // 测试正常数据
  let normal_test_values = [45.0, 50.0, 55.0, 48.0, 52.0]
  let normal_results = normal_test_values.map(fn(v) { azimuth::anomaly::detect(detector, v) })
  
  // 验证正常数据不被标记为异常
  for result in normal_results {
    assert_false(azimuth::anomaly::is_anomaly(result))
  }
  
  // 测试异常数据
  let anomaly_test_values = [10.0, 90.0, -5.0, 120.0] // 远离均值的数据
  let anomaly_results = anomaly_test_values.map(fn(v) { azimuth::anomaly::detect(detector, v) })
  
  // 验证异常数据被标记为异常
  for result in anomaly_results {
    assert_true(azimuth::anomaly::is_anomaly(result))
  }
  
  // 获取异常分数
  let anomaly_scores = anomaly_results.map(fn(r) { azimuth::anomaly::get_score(r) })
  
  // 验证异常分数
  for score in anomaly_scores {
    assert_true(score > 3.0) // 异常分数应该大于阈值
  }
}

// Test 2: 基于机器学习的异常检测
test "machine learning-based anomaly detection" {
  // 创建ML异常检测器
  let detector = azimuth::anomaly::MLDetector::new()
  
  // 配置ML模型参数
  azimuth::anomaly::set_algorithm(detector, "isolation_forest")
  azimuth::anomaly::set_contamination_rate(detector, 0.1) // 预期异常比例10%
  azimuth::anomaly::set_n_estimators(detector, 100) // 100棵树
  
  // 生成多维正常数据
  let normal_data = []
  for i in 0..=199 {
    let data_point = [
      10.0 + azimuth::random::normal(0.0, 2.0), // 特征1
      20.0 + azimuth::random::normal(0.0, 3.0), // 特征2
      5.0 + azimuth::random::normal(0.0, 1.0)   // 特征3
    ]
    normal_data = normal_data.push(data_point)
  }
  
  // 训练模型
  let train_result = azimuth::anomaly::train_ml(detector, normal_data)
  assert_true(azimuth::anomaly::is_training_successful(train_result))
  
  // 测试正常数据
  let normal_test_points = [
    [10.5, 19.8, 5.2],
    [9.8, 21.1, 4.9],
    [11.2, 20.3, 5.1]
  ]
  
  let normal_results = normal_test_points.map(fn(p) { azimuth::anomaly::detect_ml(detector, p) })
  
  // 验证正常数据不被标记为异常
  for result in normal_results {
    assert_false(azimuth::anomaly::is_anomaly(result))
  }
  
  // 测试异常数据
  let anomaly_test_points = [
    [50.0, 20.0, 5.0], // 特征1异常
    [10.0, 80.0, 5.0], // 特征2异常
    [10.0, 20.0, 30.0], // 特征3异常
    [100.0, 200.0, 50.0] // 所有特征异常
  ]
  
  let anomaly_results = anomaly_test_points.map(fn(p) { azimuth::anomaly::detect_ml(detector, p) })
  
  // 验证异常数据被标记为异常
  for result in anomaly_results {
    assert_true(azimuth::anomaly::is_anomaly(result))
  }
  
  // 获取特征重要性
  let feature_importance = azimuth::anomaly::get_feature_importance(detector)
  assert_eq(feature_importance.length(), 3)
  
  // 验证特征重要性总和为1
  let importance_sum = feature_importance.reduce(fn(acc, x) { acc + x }, 0.0)
  assert_true((importance_sum - 1.0).abs() < 0.001)
}

// Test 3: 时间序列异常检测
test "time series anomaly detection" {
  // 创建时间序列异常检测器
  let detector = azimuth::anomaly::TimeSeriesDetector::new()
  
  // 配置时间序列检测参数
  azimuth::anomaly::set_seasonality(detector, 24) // 24小时季节性
  azimuth::anomaly::set_trend_method(detector, "linear")
  azimuth::anomaly::set_threshold(detector, 2.5) // 2.5个标准差
  
  // 生成模拟时间序列数据（带有季节性和趋势）
  let time_series_data = []
  let base_value = 100.0
  let trend = 0.1
  
  for i in 0..=167 { // 一周的数据（24*7=168小时）
    let time = i.to_float()
    let seasonal = 10.0 * azimuth::math::sin(2.0 * azimuth::math::pi() * time / 24.0) // 日季节性
    let trend_value = trend * time
    let noise = azimuth::random::normal(0.0, 2.0)
    let value = base_value + seasonal + trend_value + noise
    
    time_series_data = time_series_data.push((i, value))
  }
  
  // 训练检测器
  azimuth::anomaly::train_time_series(detector, time_series_data)
  
  // 测试正常数据点
  let normal_points = [
    (24, base_value + 10.0 + trend * 24.0), // 第二天同一时间
    (48, base_value + 10.0 + trend * 48.0), // 第三天同一时间
    (72, base_value + 10.0 + trend * 72.0)  // 第四天同一时间
  ]
  
  let normal_results = normal_points.map(fn(p) { azimuth::anomaly::detect_time_series(detector, p) })
  
  // 验证正常数据不被标记为异常
  for result in normal_results {
    assert_false(azimuth::anomaly::is_anomaly(result))
  }
  
  // 测试异常数据点
  let anomaly_points = [
    (24, base_value + 50.0 + trend * 24.0), // 异常高的值
    (48, base_value - 30.0 + trend * 48.0), // 异常低的值
    (72, base_value + 10.0 + trend * 72.0 + 25.0) // 异常噪声
  ]
  
  let anomaly_results = anomaly_points.map(fn(p) { azimuth::anomaly::detect_time_series(detector, p) })
  
  // 验证异常数据被标记为异常
  for result in anomaly_results {
    assert_true(azimuth::anomaly::is_anomaly(result))
  }
  
  // 获取季节性分解结果
  let decomposition = azimuth::anomaly::get_decomposition(detector)
  let trend_component = azimuth::anomaly::decomposition::get_trend(decomposition)
  let seasonal_component = azimuth::anomaly::decomposition::get_seasonal(decomposition)
  let residual_component = azimuth::anomaly::decomposition::get_residual(decomposition)
  
  // 验证分解组件
  assert_eq(trend_component.length(), time_series_data.length())
  assert_eq(seasonal_component.length(), time_series_data.length())
  assert_eq(residual_component.length(), time_series_data.length())
}

// Test 4: 告警规则配置和管理
test "alert rule configuration and management" {
  // 创建告警管理器
  let alert_manager = azimuth::alerting::Manager::new()
  
  // 创建告警规则
  let rule1 = azimuth::alerting::Rule::new("high_error_rate")
  azimuth::alerting::rule::set_metric(rule1, "error_rate")
  azimuth::alerting::rule::set_condition(rule1, "greater_than")
  azimuth::alerting::rule::set_threshold(rule1, 0.05) // 5%错误率
  azimuth::alerting::rule::set_duration(rule1, 300) // 持续5分钟
  azimuth::alerting::rule::set_severity(rule1, "warning")
  
  let rule2 = azimuth::alerting::Rule::new("critical_latency")
  azimuth::alerting::rule::set_metric(rule2, "response_time_p99")
  azimuth::alerting::rule::set_condition(rule2, "greater_than")
  azimuth::alerting::rule::set_threshold(rule2, 1000) // 1000ms
  azimuth::alerting::rule::set_duration(rule2, 120) // 持续2分钟
  azimuth::alerting::rule::set_severity(rule2, "critical")
  
  let rule3 = azimuth::alerting::Rule::new("low_throughput")
  azimuth::alerting::rule::set_metric(rule3, "requests_per_second")
  azimuth::alerting::rule::set_condition(rule3, "less_than")
  azimuth::alerting::rule::set_threshold(rule3, 10) // 10 RPS
  azimuth::alerting::rule::set_duration(rule3, 600) // 持续10分钟
  azimuth::alerting::rule::set_severity(rule3, "warning")
  
  // 注册告警规则
  azimuth::alerting::register_rule(alert_manager, rule1)
  azimuth::alerting::register_rule(alert_manager, rule2)
  azimuth::alerting::register_rule(alert_manager, rule3)
  
  // 获取已注册的规则
  let registered_rules = azimuth::alerting::get_rules(alert_manager)
  assert_eq(registered_rules.length(), 3)
  
  // 创建测试指标数据
  let test_metrics = [
    ("error_rate", 0.08, azimuth::time::now()), // 超过阈值
    ("response_time_p99", 1200, azimuth::time::now()), // 超过阈值
    ("requests_per_second", 5, azimuth::time::now()), // 低于阈值
    ("cpu_usage", 0.6, azimuth::time::now()) // 不在规则中
  ]
  
  // 评估告警规则
  let alert_results = []
  for (metric, value, timestamp) in test_metrics {
    let result = azimuth::alerting::evaluate_rules(alert_manager, metric, value, timestamp)
    alert_results = alert_results.push(result)
  }
  
  // 验证告警结果
  let error_rate_alerts = alert_results[0]
  let latency_alerts = alert_results[1]
  let throughput_alerts = alert_results[2]
  let cpu_alerts = alert_results[3]
  
  assert_eq(error_rate_alerts.length(), 1) // 应该触发一个告警
  assert_eq(latency_alerts.length(), 1) // 应该触发一个告警
  assert_eq(throughput_alerts.length(), 1) // 应该触发一个告警
  assert_eq(cpu_alerts.length(), 0) // 不应该触发告警
  
  // 验证告警详情
  let error_rate_alert = error_rate_alerts[0]
  assert_eq(azimuth::alerting::alert::get_rule_name(error_rate_alert), "high_error_rate")
  assert_eq(azimuth::alerting::alert::get_severity(error_rate_alert), "warning")
  assert_eq(azimuth::alerting::alert::get_value(error_rate_alert), 0.08)
  assert_eq(azimuth::alerting::alert::get_threshold(error_rate_alert), 0.05)
  
  let latency_alert = latency_alerts[0]
  assert_eq(azimuth::alerting::alert::get_rule_name(latency_alert), "critical_latency")
  assert_eq(azimuth::alerting::alert::get_severity(latency_alert), "critical")
}

// Test 5: 告警通知和分发
test "alert notification and distribution" {
  // 创建通知管理器
  let notification_manager = azimuth::alerting::NotificationManager::new()
  
  // 配置通知渠道
  let email_channel = azimuth::alerting::EmailChannel::new("smtp.example.com", 587)
  azimuth::alerting::email::set_credentials(email_channel, "alerts@example.com", "password")
  azimuth::alerting::email::add_recipient(email_channel, "admin@example.com")
  azimuth::alerting::email::add_recipient(email_channel, "ops@example.com")
  
  let slack_channel = azimuth::alerting::SlackChannel::new("https://hooks.slack.com/services/xxx")
  azimuth::alerting::slack::set_channel(slack_channel, "#alerts")
  azimuth::alerting::slack::set_username(slack_channel, "Azimuth AlertBot")
  
  let pagerduty_channel = azimuth::alerting::PagerDutyChannel::new("integration-key")
  azimuth::alerting::pagerduty::set_severity_mapping(pagerduty_channel, [
    ("critical", "critical"),
    ("warning", "warning"),
    ("info", "info")
  ])
  
  // 注册通知渠道
  azimuth::alerting::register_channel(notification_manager, "email", email_channel)
  azimuth::alerting::register_channel(notification_manager, "slack", slack_channel)
  azimuth::alerting::register_channel(notification_manager, "pagerduty", pagerduty_channel)
  
  // 创建告警
  let critical_alert = azimuth::alerting::Alert::new("critical_latency", "critical")
  azimuth::alerting::alert::set_message(critical_alert, "P99 latency exceeded 1000ms")
  azimuth::alerting::alert::set_value(critical_alert, 1200.0)
  azimuth::alerting::alert::set_threshold(critical_alert, 1000.0)
  azimuth::alerting::alert::set_timestamp(critical_alert, azimuth::time::now())
  
  let warning_alert = azimuth::alerting::Alert::new("high_error_rate", "warning")
  azimuth::alerting::alert::set_message(warning_alert, "Error rate exceeded 5%")
  azimuth::alerting::alert::set_value(warning_alert, 0.08)
  azimuth::alerting::alert::set_threshold(warning_alert, 0.05)
  azimuth::alerting::alert::set_timestamp(warning_alert, azimuth::time::now())
  
  // 配置告警路由规则
  azimuth::alerting::add_route_rule(notification_manager, "severity", "critical", ["pagerduty", "slack", "email"])
  azimuth::alerting::add_route_rule(notification_manager, "severity", "warning", ["slack", "email"])
  azimuth::alerting::add_route_rule(notification_manager, "rule_name", "high_error_rate", ["email"])
  
  // 发送告警通知
  let critical_notification_result = azimuth::alerting::send_notification(notification_manager, critical_alert)
  let warning_notification_result = azimuth::alerting::send_notification(notification_manager, warning_alert)
  
  // 验证通知结果
  assert_true(azimuth::alerting::notification::is_successful(critical_notification_result))
  assert_true(azimuth::alerting::notification::is_successful(warning_notification_result))
  
  // 获取通知详情
  let critical_channels = azimuth::alerting::notification::get_channels(critical_notification_result)
  let warning_channels = azimuth::alerting::notification::get_channels(warning_notification_result)
  
  assert_eq(critical_channels.length(), 3) // 严重告警应该发送到3个渠道
  assert_eq(warning_channels.length(), 2) // 警告告警应该发送到2个渠道
  
  assert_true(critical_channels.contains("pagerduty"))
  assert_true(critical_channels.contains("slack"))
  assert_true(critical_channels.contains("email"))
  
  assert_true(warning_channels.contains("slack"))
  assert_true(warning_channels.contains("email"))
  
  // 获取通知统计
  let notification_stats = azimuth::alerting::get_notification_statistics(notification_manager)
  let total_notifications = azimuth::alerting::stats::get_total_sent(notification_stats)
  let email_notifications = azimuth::alerting::stats::get_sent_by_channel(notification_stats, "email")
  let slack_notifications = azimuth::alerting::stats::get_sent_by_channel(notification_stats, "slack")
  let pagerduty_notifications = azimuth::alerting::stats::get_sent_by_channel(notification_stats, "pagerduty")
  
  assert_eq(total_notifications, 5) // 3 + 2 = 5个通知
  assert_eq(email_notifications, 2) // 2个邮件通知
  assert_eq(slack_notifications, 2) // 2个Slack通知
  assert_eq(pagerduty_notifications, 1) // 1个PagerDuty通知
}

// Test 6: 告警抑制和分组
test "alert suppression and grouping" {
  // 创建告警管理器
  let alert_manager = azimuth::alerting::Manager::new()
  
  // 配置告警抑制规则
  azimuth::alerting::add_suppression_rule(alert_manager, "maintenance_window", [
    ("environment", "production"),
    ("maintenance_mode", "true")
  ], 3600) // 维护期间抑制1小时
  
  azimuth::alerting::add_suppression_rule(alert_manager, "known_issue", [
    ("error_type", "timeout"),
    ("service", "legacy-service")
  ], 1800) // 已知问题抑制30分钟
  
  // 配置告警分组规则
  azimuth::alerting::add_grouping_rule(alert_manager, "service_group", [
    "service"
  ], 300) // 按服务分组，5分钟内的告警合并
  
  azimuth::alerting::add_grouping_rule(alert_manager, "host_group", [
    "host",
    "severity"
  ], 600) // 按主机和严重性分组，10分钟内的告警合并
  
  // 创建测试告警
  let alerts = [
    // 正常告警
    azimuth::alerting::create_test_alert("high_cpu", "warning", [
      ("service", "web-server"),
      ("host", "host-1"),
      ("environment", "production")
    ]),
    
    // 应该被抑制的告警（维护窗口）
    azimuth::alerting::create_test_alert("high_memory", "warning", [
      ("service", "web-server"),
      ("host", "host-2"),
      ("environment", "production"),
      ("maintenance_mode", "true")
    ]),
    
    // 应该被抑制的告警（已知问题）
    azimuth::alerting::create_test_alert("service_timeout", "critical", [
      ("service", "legacy-service"),
      ("host", "host-3"),
      ("error_type", "timeout")
    ]),
    
    // 应该被分组的告警（同一服务）
    azimuth::alerting::create_test_alert("disk_space", "warning", [
      ("service", "web-server"),
      ("host", "host-4")
    ]),
    
    // 应该被分组的告警（同一服务和主机）
    azimuth::alerting::create_test_alert("network_error", "warning", [
      ("service", "web-server"),
      ("host", "host-4")
    ])
  ]
  
  // 处理告警
  let processed_alerts = []
  for alert in alerts {
    let processed = azimuth::alerting::process_alert(alert_manager, alert)
    processed_alerts = processed_alerts.push(processed)
  }
  
  // 验证告警抑制
  let suppressed_alerts = processed_alerts.filter(fn(a) { azimuth::alerting::is_suppressed(a) })
  assert_eq(suppressed_alerts.length(), 2) // 应该有2个告警被抑制
  
  // 验证告警分组
  let grouped_alerts = processed_alerts.filter(fn(a) { azimuth::alerting::is_grouped(a) })
  assert_eq(grouped_alerts.length(), 2) // 应该有2个告警被分组
  
  // 获取告警组
  let alert_groups = azimuth::alerting::get_alert_groups(alert_manager)
  assert_eq(alert_groups.length(), 2) // 应该有2个告警组
  
  // 验证服务分组
  let service_group = alert_groups.find(fn(g) { azimuth::alerting::group::get_name(g) == "service_group" }).unwrap()
  let service_group_alerts = azimuth::alerting::group::get_alerts(service_group)
  assert_eq(service_group_alerts.length(), 3) // web-server的3个告警
  
  // 验证主机分组
  let host_group = alert_groups.find(fn(g) { azimuth::alerting::group::get_name(g) == "host_group" }).unwrap()
  let host_group_alerts = azimuth::alerting::group::get_alerts(host_group)
  assert_eq(host_group_alerts.length(), 2) // host-4的2个告警
}

// Test 7: 告警升级和自动恢复
test "alert escalation and auto-recovery" {
  // 创建告警管理器
  let alert_manager = azimuth::alerting::Manager::new()
  
  // 配置告警升级规则
  let escalation_rule = azimuth::alerting::EscalationRule::new("critical_escalation")
  azimuth::alerting::escalation::add_level(escalation_rule, 1, [
    ("channel", "email"),
    ("recipients", ["team-lead@example.com"])
  ], 300) // 5分钟后升级到第一级
  
  azimuth::alerting::escalation::add_level(escalation_rule, 2, [
    ("channel", "slack"),
    ("recipients", ["#ops-alerts"])
  ], 600) // 10分钟后升级到第二级
  
  azimuth::alerting::escalation::add_level(escalation_rule, 3, [
    ("channel", "pagerduty"),
    ("recipients", ["on-call-engineer"])
  ], 900) // 15分钟后升级到第三级
  
  // 注册升级规则
  azimuth::alerting::register_escalation_rule(alert_manager, "critical", escalation_rule)
  
  // 配置自动恢复规则
  let recovery_rule = azimuth::alerting::RecoveryRule::new("auto_recovery")
  azimuth::alerting::recovery::set_condition(recovery_rule, "metric_normal_for", 300) // 指标正常5分钟后恢复
  azimuth::alerting::recovery::set_recovery_message(recovery_rule, "Alert automatically resolved")
  
  // 注册恢复规则
  azimuth::alerting::register_recovery_rule(alert_manager, "high_error_rate", recovery_rule)
  
  // 创建持续告警
  let persistent_alert = azimuth::alerting::Alert::new("high_error_rate", "critical")
  azimuth::alerting::alert::set_message(persistent_alert, "Error rate is critically high")
  azimuth::alerting::alert::set_timestamp(persistent_alert, azimuth::time::now() - 1200) // 20分钟前开始
  
  // 处理告警升级
  let escalation_result = azimuth::alerting::process_escalation(alert_manager, persistent_alert)
  
  // 验证升级状态
  let current_level = azimuth::alerting::escalation::get_current_level(escalation_result)
  assert_eq(current_level, 3) // 应该已经升级到第三级
  
  let escalation_history = azimuth::alerting::escalation::get_history(escalation_result)
  assert_eq(escalation_history.length(), 3) // 应该有3次升级记录
  
  // 模拟指标恢复正常
  let normal_metrics = [
    ("error_rate", 0.01, azimuth::time::now() - 300), // 5分钟前恢复正常
    ("error_rate", 0.02, azimuth::time::now() - 240), // 4分钟前
    ("error_rate", 0.015, azimuth::time::now() - 180), // 3分钟前
    ("error_rate", 0.008, azimuth::time::now() - 120), // 2分钟前
    ("error_rate", 0.012, azimuth::time::now() - 60),  // 1分钟前
    ("error_rate", 0.01, azimuth::time::now())          // 现在
  ]
  
  // 处理自动恢复
  let recovery_result = azimuth::alerting::process_recovery(alert_manager, persistent_alert, normal_metrics)
  
  // 验证恢复结果
  assert_true(azimuth::alerting::recovery::is_recovered(recovery_result))
  assert_eq(azimuth::alerting::recovery::get_reason(recovery_result), "metric_normal_for")
  
  // 获取恢复通知
  let recovery_notification = azimuth::alerting::recovery::get_notification(recovery_result)
  assert_eq(azimuth::alerting::notification::get_message(recovery_notification), "Alert automatically resolved")
  
  // 获取告警生命周期统计
  let lifecycle_stats = azimuth::alerting::get_lifecycle_statistics(alert_manager)
  let escalated_alerts = azimuth::alerting::stats::get_escalated_alerts(lifecycle_stats)
  let recovered_alerts = azimuth::alerting::stats::get_auto_recovered_alerts(lifecycle_stats)
  
  assert_eq(escalated_alerts, 1)
  assert_eq(recovered_alerts, 1)
}

// Test 8: 告警性能和负载测试
test "alert performance and load testing" {
  // 创建高性能告警管理器
  let alert_manager = azimuth::alerting::HighPerformanceManager::new()
  
  // 配置性能参数
  azimuth::alerting::set_max_concurrent_evaluations(alert_manager, 100)
  azimuth::alerting::set_evaluation_timeout(alert_manager, 100) // 100ms超时
  azimuth::alerting::enable_batch_processing(alert_manager, true)
  azimuth::alerting::set_batch_size(alert_manager, 50)
  
  // 创建大量告警规则
  for i in 0..=99 {
    let rule = azimuth::alerting::Rule::new("rule_" + i.to_string())
    azimuth::alerting::rule::set_metric(rule, "metric_" + i.to_string())
    azimuth::alerting::rule::set_condition(rule, "greater_than")
    azimuth::alerting::rule::set_threshold(rule, i.to_float())
    azimuth::alerting::rule::set_severity(rule, if i % 10 == 0 { "critical" } else { "warning" })
    
    azimuth::alerting::register_rule(alert_manager, rule)
  }
  
  // 生成大量指标数据
  let metrics_data = []
  for i in 0..=9999 {
    let metric_name = "metric_" + (i % 100).to_string()
    let value = (i % 100).to_float() + azimuth::random::uniform(-5.0, 5.0)
    let timestamp = azimuth::time::now() - (i * 1000) // 每秒一个数据点
    
    metrics_data = metrics_data.push((metric_name, value, timestamp))
  }
  
  // 测量批量处理性能
  let start_time = azimuth::time::now()
  
  let batch_results = azimuth::alerting::evaluate_batch(alert_manager, metrics_data)
  
  let end_time = azimuth::time::now()
  let processing_time = end_time - start_time
  
  // 验证性能
  assert_true(processing_time < 5000) // 应该在5秒内完成
  assert_eq(batch_results.length(), metrics_data.length())
  
  // 统计告警数量
  let total_alerts = batch_results.reduce(fn(acc, results) { acc + results.length() }, 0)
  let critical_alerts = batch_results.reduce(fn(acc, results) {
    acc + results.filter(fn(r) { azimuth::alerting::alert::get_severity(r) == "critical" }).length()
  }, 0)
  
  assert_true(total_alerts > 0) // 应该有告警产生
  assert_true(critical_alerts > 0) // 应该有严重告警
  
  // 获取性能统计
  let performance_stats = azimuth::alerting::get_performance_statistics(alert_manager)
  let evaluations_per_second = azimuth::alerting::stats::get_evaluations_per_second(performance_stats)
  let average_evaluation_time = azimuth::alerting::stats::get_average_evaluation_time(performance_stats)
  let memory_usage = azimuth::alerting::stats::get_memory_usage(performance_stats)
  
  assert_true(evaluations_per_second > 1000) // 每秒应该能处理1000+评估
  assert_true(average_evaluation_time < 10) // 平均评估时间应该小于10ms
  assert_true(memory_usage > 0) // 应该有内存使用统计
  
  // 测试内存清理
  let memory_before = azimuth::alerting::stats::get_memory_usage(performance_stats)
  azimuth::alerting::cleanup_memory(alert_manager)
  
  let performance_stats_after = azimuth::alerting::get_performance_statistics(alert_manager)
  let memory_after = azimuth::alerting::stats::get_memory_usage(performance_stats_after)
  
  assert_true(memory_after < memory_before) // 内存使用应该减少
}