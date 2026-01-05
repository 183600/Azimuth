// Azimuth 实时告警系统测试
// 专注于实时告警的触发、路由、抑制和恢复机制

// 测试1: 实时告警触发机制
test "实时告警触发机制" {
  // 创建实时告警管理器
  let alert_manager = RealTimeAlertManager::new()
  
  // 配置告警规则
  AlertManager::add_rule(alert_manager, "high_error_rate", {
    name: "High Error Rate",
    description: "Error rate exceeds threshold",
    condition: "error_rate > 0.05",
    severity: "critical",
    evaluation_interval: 30,  // 30秒评估一次
    for_duration: 60,         // 持续60秒才触发
    labels: ["service", "environment"],
    annotations: ["runbook_url"]
  })
  
  AlertManager::add_rule(alert_manager, "high_latency", {
    name: "High Latency",
    description: "Response latency exceeds threshold",
    condition: "response_time_p95 > 500",
    severity: "warning",
    evaluation_interval: 15,
    for_duration: 30,
    labels: ["service", "endpoint"],
    annotations: ["sla_impact"]
  })
  
  AlertManager::add_rule(alert_manager, "resource_exhaustion", {
    name: "Resource Exhaustion",
    description: "System resources exhausted",
    condition: "cpu_usage > 90 or memory_usage > 95",
    severity: "critical",
    evaluation_interval: 10,
    for_duration: 15,
    labels: ["host", "resource_type"],
    annotations: ["escalation_policy"]
  })
  
  // 创建实时数据流
  let data_stream = RealTimeDataStream::new()
  let base_time = 1640995200
  
  // 模拟正常数据流
  for i in 0..=10 {
    RealTimeDataStream::add_data_point(data_stream, base_time + i * 30, [
      ("service", StringValue("api.service")),
      ("error_rate", DoubleValue(0.01)),
      ("response_time_p95", DoubleValue(200.0)),
      ("cpu_usage", DoubleValue(45.0)),
      ("memory_usage", DoubleValue(60.0))
    ])
  }
  
  // 评估告警规则（应该没有告警）
  let normal_evaluation = AlertManager::evaluate_rules(alert_manager, data_stream)
  assert_eq(normal_evaluation.active_alerts.length(), 0)
  assert_eq(normal_evaluation.pending_alerts.length(), 0)
  
  // 模拟错误率逐渐升高
  for i in 11..=15 {
    let error_rate = 0.02 + (i - 10) * 0.01
    RealTimeDataStream::add_data_point(data_stream, base_time + i * 30, [
      ("service", StringValue("api.service")),
      ("error_rate", DoubleValue(error_rate)),
      ("response_time_p95", DoubleValue(250.0)),
      ("cpu_usage", DoubleValue(50.0)),
      ("memory_usage", DoubleValue(65.0))
    ])
  }
  
  // 评估告警规则（应该有告警处于pending状态）
  let pending_evaluation = AlertManager::evaluate_rules(alert_manager, data_stream)
  assert_eq(pending_evaluation.active_alerts.length(), 0)
  assert_eq(pending_evaluation.pending_alerts.length(), 1)
  
  let pending_alert = pending_evaluation.pending_alerts[0]
  assert_eq(pending_alert.rule_name, "high_error_rate")
  assert_eq(pending_alert.severity, "critical")
  assert_true(pending_alert.condition_met)
  assert_true(pending_alert.firing_duration < 60)  // 还未达到触发持续时间
  
  // 继续添加数据，使告警触发
  for i in 16..=20 {
    let error_rate = 0.07 + (i - 15) * 0.005
    RealTimeDataStream::add_data_point(data_stream, base_time + i * 30, [
      ("service", StringValue("api.service")),
      ("error_rate", DoubleValue(error_rate)),
      ("response_time_p95", DoubleValue(300.0)),
      ("cpu_usage", DoubleValue(55.0)),
      ("memory_usage", DoubleValue(70.0))
    ])
  }
  
  // 评估告警规则（应该有活跃告警）
  let active_evaluation = AlertManager::evaluate_rules(alert_manager, data_stream)
  assert_eq(active_evaluation.active_alerts.length(), 1)
  assert_eq(active_evaluation.pending_alerts.length(), 0)
  
  let active_alert = active_evaluation.active_alerts[0]
  assert_eq(active_alert.rule_name, "high_error_rate")
  assert_eq(active_alert.severity, "critical")
  assert_true(active_alert.condition_met)
  assert_true(active_alert.firing_duration >= 60)
  assert_true(active_alert.triggered_at > 0)
  
  // 验证告警上下文
  assert_eq(active_alert.context.service, "api.service")
  assert_eq(active_alert.context.current_value, 0.095)
  assert_eq(active_alert.context.threshold_value, 0.05)
  assert_true(active_alert.context.evaluation_count > 0)
  
  // 测试多条件告警
  for i in 21..=25 {
    RealTimeDataStream::add_data_point(data_stream, base_time + i * 30, [
      ("service", StringValue("api.service")),
      ("error_rate", DoubleValue(0.03)),
      ("response_time_p95", DoubleValue(600.0)),
      ("cpu_usage", DoubleValue(92.0)),
      ("memory_usage", DoubleValue(75.0))
    ])
  }
  
  // 评估多条件告警
  let multi_condition_evaluation = AlertManager::evaluate_rules(alert_manager, data_stream)
  assert_true(multi_condition_evaluation.active_alerts.length() >= 2)
  
  // 验证资源耗尽告警
  let resource_alert = multi_condition_evaluation.active_alerts.find(fn(a) { a.rule_name == "resource_exhaustion" })
  assert_true(resource_alert != None)
  
  match resource_alert {
    Some(alert) => {
      assert_eq(alert.severity, "critical")
      assert_true(alert.context.current_value > 90.0)
      assert_eq(alert.context.resource_type, "cpu")
    }
    None => assert_true(false)
  }
}

// 测试2: 告警路由和分发
test "告警路由和分发机制" {
  // 创建告警路由器
  let alert_router = AlertRouter::new()
  
  // 配置路由规则
  AlertRouter::add_route(alert_router, "service_critical", {
    name: "Service Critical Alerts",
    conditions: [
      { field: "severity", operator: "=", value: "critical" },
      { field: "service", operator: "in", value: ["api.service", "auth.service", "data.service"] }
    ],
    targets: [
      { type: "email", recipients: ["oncall@company.com", "team-lead@company.com"] },
      { type: "slack", channel: "#alerts-critical", mention: ["@oncall"] },
      { type: "pagerduty", service_key: "critical-service-key" }
    ],
    escalation_policy: "immediate"
  })
  
  AlertRouter::add_route(alert_router, "infrastructure_warnings", {
    name: "Infrastructure Warning Alerts",
    conditions: [
      { field: "severity", operator: "=", value: "warning" },
      { field: "category", operator: "=", value: "infrastructure" }
    ],
    targets: [
      { type: "email", recipients: ["infra-team@company.com"] },
      { type: "slack", channel: "#infra-alerts" }
    ],
    escalation_policy: "5_minutes"
  })
  
  AlertRouter::add_route(alert_router, "business_impact", {
    name: "Business Impact Alerts",
    conditions: [
      { field: "business_impact", operator: "=", value: "high" }
    ],
    targets: [
      { type: "email", recipients: ["business-team@company.com", "executives@company.com"] },
      { type: "slack", channel: "#business-alerts", mention: ["@executives"] },
      { type: "webhook", url: "https://incident-management.company.com/api/alerts" }
    ],
    escalation_policy: "2_minutes"
  })
  
  // 创建测试告警
  let critical_service_alert = {
    id: "alert-001",
    rule_name: "high_error_rate",
    severity: "critical",
    service: "api.service",
    category: "application",
    business_impact: "high",
    timestamp: 1640995200,
    message: "Error rate exceeded 5% threshold",
    context: {
      current_value: 0.08,
      threshold_value: 0.05,
      affected_endpoints: ["/api/users", "/api/orders"]
    }
  }
  
  let infra_warning_alert = {
    id: "alert-002",
    rule_name: "disk_space_low",
    severity: "warning",
    service: "monitoring.service",
    category: "infrastructure",
    business_impact: "medium",
    timestamp: 1640995210,
    message: "Disk space below 20% threshold",
    context: {
      current_value: 15.5,
      threshold_value: 20.0,
      mount_point: "/var/log"
    }
  }
  
  let business_impact_alert = {
    id: "alert-003",
    rule_name: "payment_processing_failure",
    severity: "critical",
    service: "payment.service",
    category: "application",
    business_impact: "high",
    timestamp: 1640995220,
    message: "Payment processing failure rate exceeded 1%",
    context: {
      current_value: 0.015,
      threshold_value: 0.01,
      estimated_revenue_impact: 5000.0
    }
  }
  
  // 路由告警
  let critical_route_result = AlertRouter::route_alert(alert_router, critical_service_alert)
  let infra_route_result = AlertRouter::route_alert(alert_router, infra_warning_alert)
  let business_route_result = AlertRouter::route_alert(alert_router, business_impact_alert)
  
  // 验证关键服务告警路由
  assert_eq(critical_route_result.matched_routes.length(), 2)  // service_critical + business_impact
  assert_true(critical_route_result.matched_routes.any(fn(r) { r.name == "Service Critical Alerts" }))
  assert_true(critical_route_result.matched_routes.any(fn(r) { r.name == "Business Impact Alerts" }))
  
  let service_critical_route = critical_route_result.matched_routes.find(fn(r) { r.name == "Service Critical Alerts" })
  assert_true(service_critical_route != None)
  
  match service_critical_route {
    Some(route) => {
      assert_eq(route.targets.length(), 3)  // email + slack + pagerduty
      assert_true(route.targets.any(fn(t) { t.type == "email" }))
      assert_true(route.targets.any(fn(t) { t.type == "slack" }))
      assert_true(route.targets.any(fn(t) { t.type == "pagerduty" }))
      assert_eq(route.escalation_policy, "immediate")
    }
    None => assert_true(false)
  }
  
  // 验证基础设施告警路由
  assert_eq(infra_route_result.matched_routes.length(), 1)
  assert_eq(infra_route_result.matched_routes[0].name, "Infrastructure Warning Alerts")
  
  // 验证业务影响告警路由
  assert_eq(business_route_result.matched_routes.length(), 2)  // service_critical + business_impact
  
  // 测试告警分发
  let notification_service = NotificationService::new()
  
  // 分发关键服务告警
  let critical_dispatch_result = AlertRouter::dispatch_alerts(alert_router, critical_service_alert, notification_service)
  
  // 验证分发结果
  assert_eq(critical_dispatch_result.total_notifications, 6)  // 2 routes × 3 targets
  assert_eq(critical_dispatch_result.successful_notifications, 6)
  assert_eq(critical_dispatch_result.failed_notifications, 0)
  
  // 验证通知详情
  let email_notifications = critical_dispatch_result.notifications.filter(fn(n) { n.type == "email" })
  assert_eq(email_notifications.length(), 2)
  
  let slack_notifications = critical_dispatch_result.notifications.filter(fn(n) { n.type == "slack" })
  assert_eq(slack_notifications.length(), 2)
  
  let pagerduty_notifications = critical_dispatch_result.notifications.filter(fn(n) { n.type == "pagerduty" })
  assert_eq(pagerduty_notifications.length(), 1)
  
  let webhook_notifications = critical_dispatch_result.notifications.filter(fn(n) { n.type == "webhook" })
  assert_eq(webhook_notifications.length(), 1)
  
  // 验证通知内容
  let first_email = email_notifications[0]
  assert_true(first_email.recipients.contains("oncall@company.com"))
  assert_true(first_email.subject.contains("critical"))
  assert_true(first_email.body.contains("api.service"))
  
  // 测试分发失败处理
  let failing_notification_service = FailingNotificationService::new()
  let failed_dispatch_result = AlertRouter::dispatch_alerts(alert_router, critical_service_alert, failing_notification_service)
  
  // 验证失败处理
  assert_eq(failed_dispatch_result.total_notifications, 6)
  assert_eq(failed_dispatch_result.successful_notifications, 0)
  assert_eq(failed_dispatch_result.failed_notifications, 6)
  assert_true(failed_dispatch_result.retry_scheduled)
}

// 测试3: 告警抑制和分组
test "告警抑制和分组机制" {
  // 创建告警抑制管理器
  let alert_suppressor = AlertSuppressor::new()
  
  // 配置抑制规则
  AlertSuppressor::add_rule(alert_suppressor, "cascade_suppression", {
    name: "Cascade Alert Suppression",
    description: "Suppress downstream alerts when upstream service fails",
    conditions: [
      { field: "service", operator: "=", value: "api.gateway" },
      { field: "severity", operator: "=", value: "critical" }
    ],
    suppression_duration: 600,  // 10分钟
    suppressed_rules: [
      { service: "auth.service", severity: "any" },
      { service: "data.service", severity: "any" },
      { service: "cache.service", severity: "warning" }
    ]
  })
  
  AlertSuppressor::add_rule(alert_suppressor, "maintenance_suppression", {
    name: "Maintenance Window Suppression",
    description: "Suppress alerts during maintenance windows",
    conditions: [
      { field: "maintenance_mode", operator: "=", value: "true" }
    ],
    suppression_duration: 3600,  // 1小时
    suppressed_rules: [
      { service: "any", severity: "warning" }
    ]
  })
  
  AlertSuppressor::add_rule(alert_suppressor, "rate_limit_suppression", {
    name: "Rate Limit Suppression",
    description: "Suppress similar alerts within rate limit",
    conditions: [
      { field: "rule_name", operator: "=", value: "high_error_rate" }
    ],
    suppression_duration: 300,  // 5分钟
    suppression_criteria: [
      { field: "service", operator: "=" },
      { field: "severity", operator: "=" }
    ]
  })
  
  // 创建告警分组器
  let alert_grouper = AlertGrouper::new()
  
  // 配置分组规则
  AlertGrouper::add_rule(alert_grouper, "service_grouping", {
    name: "Service-based Grouping",
    group_by: ["service", "severity"],
    group_interval: 300,  // 5分钟
    max_group_size: 10,
    group_threshold: 3    // 至少3个告警才分组
  })
  
  AlertGrouper::add_rule(alert_grouper, "cluster_grouping", {
    name: "Cluster-based Grouping",
    group_by: ["cluster", "category"],
    group_interval: 600,  // 10分钟
    max_group_size: 20,
    group_threshold: 5
  })
  
  // 创建测试告警序列
  let alert_sequence = []
  let base_time = 1640995200
  
  // 1. API网关关键告警
  alert_sequence = alert_sequence.push({
    id: "alert-001",
    rule_name: "service_down",
    service: "api.gateway",
    severity: "critical",
    timestamp: base_time,
    cluster: "us-east-1",
    category: "application"
  })
  
  // 2. 下游服务告警（应该被抑制）
  alert_sequence = alert_sequence.push({
    id: "alert-002",
    rule_name: "high_error_rate",
    service: "auth.service",
    severity: "critical",
    timestamp: base_time + 30,
    cluster: "us-east-1",
    category: "application"
  })
  
  alert_sequence = alert_sequence.push({
    id: "alert-003",
    rule_name: "high_latency",
    service: "data.service",
    severity: "warning",
    timestamp: base_time + 60,
    cluster: "us-east-1",
    category: "application"
  })
  
  // 3. 维护模式告警
  alert_sequence = alert_sequence.push({
    id: "alert-004",
    rule_name: "maintenance_mode",
    service: "system",
    severity: "info",
    timestamp: base_time + 90,
    cluster: "us-east-1",
    category: "system",
    maintenance_mode: "true"
  })
  
  // 4. 维护期间的警告告警（应该被抑制）
  alert_sequence = alert_sequence.push({
    id: "alert-005",
    rule_name: "high_memory_usage",
    service: "cache.service",
    severity: "warning",
    timestamp: base_time + 120,
    cluster: "us-east-1",
    category: "infrastructure"
  })
  
  // 5. 重复告警（应该被抑制）
  alert_sequence = alert_sequence.push({
    id: "alert-006",
    rule_name: "high_error_rate",
    service: "api.service",
    severity: "critical",
    timestamp: base_time + 150,
    cluster: "us-east-1",
    category: "application"
  })
  
  alert_sequence = alert_sequence.push({
    id: "alert-007",
    rule_name: "high_error_rate",
    service: "api.service",
    severity: "critical",
    timestamp: base_time + 180,
    cluster: "us-east-1",
    category: "application"
  })
  
  // 6. 更多告警用于分组测试
  for i in 0..=5 {
    alert_sequence = alert_sequence.push({
      id: "alert-" + (8 + i).to_string(),
      rule_name: "pod_restart",
      service: "worker.service",
      severity: "warning",
      timestamp: base_time + 210 + i * 30,
      cluster: "us-east-1",
      category: "infrastructure"
    })
  }
  
  // 应用抑制规则
  let suppression_result = AlertSuppressor::apply_suppression(alert_suppressor, alert_sequence)
  
  // 验证抑制结果
  assert_eq(suppression_result.total_alerts, 13)
  assert_true(suppression_result.suppressed_alerts > 0)
  assert_true(suppression_result.active_alerts < suppression_result.total_alerts)
  
  // 验证具体抑制的告警
  let suppressed_alerts = suppression_result.suppressed_details
  assert_true(suppressed_alerts.length() > 0)
  
  // 验证级联抑制
  let cascade_suppressed = suppressed_alerts.filter(fn(s) { s.rule == "cascade_suppression" })
  assert_true(cascade_suppressed.length() >= 2)  // auth.service和data.service告警被抑制
  
  // 验证维护模式抑制
  let maintenance_suppressed = suppressed_alerts.filter(fn(s) { s.rule == "maintenance_suppression" })
  assert_true(maintenance_suppressed.length() >= 1)  // cache.service告警被抑制
  
  // 验证速率限制抑制
  let rate_limit_suppressed = suppressed_alerts.filter(fn(s) { s.rule == "rate_limit_suppression" })
  assert_true(rate_limit_suppressed.length() >= 1)  // 重复的high_error_rate告警被抑制
  
  // 应用分组规则
  let grouping_result = AlertGrouper::apply_grouping(alert_grouper, suppression_result.active_alerts)
  
  // 验证分组结果
  assert_true(grouping_result.groups.length() > 0)
  assert_true(grouping_result.ungrouped_alerts.length() >= 0)
  
  // 验证服务分组
  let service_groups = grouping_result.groups.filter(fn(g) { g.grouping_rule == "service_grouping" })
  assert_true(service_groups.length() > 0)
  
  let worker_service_group = service_groups.find(fn(g) { g.group_key.contains("worker.service") })
  assert_true(worker_service_group != None)
  
  match worker_service_group {
    Some(group) => {
      assert_true(group.alerts.length() >= 3)  // 至少3个worker.service告警
      assert_eq(group.grouping_criteria["service"], "worker.service")
      assert_eq(group.grouping_criteria["severity"], "warning")
    }
    None => assert_true(false)
  }
  
  // 验证分组告警内容
  let group_alert = grouping_result.groups[0]
  assert_true(group_alert.id.length() > 0)
  assert_true(group_alert.title.contains("Grouped Alert"))
  assert_true(group_alert.alert_count > 1)
  assert_true(group_alert.member_alerts.length() > 1)
  
  // 测试分组告警的恢复
  let group_recovery_result = AlertGrouper::handle_group_recovery(alert_grouper, group_alert.id)
  
  // 验证分组恢复
  assert_true(group_recovery_result.group_resolved)
  assert_eq(group_recovery_result.resolved_alerts, group_alert.alert_count)
  assert_true(group_recovery_result.recovery_timestamp > 0)
}

// 测试4: 告警升级和恢复
test "告警升级和恢复机制" {
  // 创建告警升级管理器
  let escalation_manager = AlertEscalationManager::new()
  
  // 配置升级策略
  EscalationManager::add_policy(escalation_manager, "critical_service", {
    name: "Critical Service Escalation",
    description: "Escalation policy for critical service alerts",
    levels: [
      {
        level: 1,
        delay: 0,              // 立即
        targets: [
          { type: "pagerduty", service_key: "oncall-engineering" },
          { type: "slack", channel: "#oncall", mention: ["@oncall-engineer"] }
        ],
        auto_resolve: false
      },
      {
        level: 2,
        delay: 300,            // 5分钟后
        targets: [
          { type: "pagerduty", service_key: "team-lead" },
          { type: "email", recipients: ["team-lead@company.com"] },
          { type: "slack", channel: "#alerts", mention: ["@team-lead"] }
        ],
        auto_resolve: false
      },
      {
        level: 3,
        delay: 900,            // 15分钟后
        targets: [
          { type: "pagerduty", service_key: "engineering-manager" },
          { type: "email", recipients: ["eng-manager@company.com"] },
          { type: "slack", channel: "#management", mention: ["@eng-manager"] },
          { type: "phone", recipients: ["+1234567890"] }
        ],
        auto_resolve: false
      }
    ],
    max_escalation_level: 3,
    repeat_interval: 1800      // 30分钟重复通知
  })
  
  EscalationManager::add_policy(escalation_manager, "business_impact", {
    name: "Business Impact Escalation",
    description: "Escalation policy for high business impact alerts",
    levels: [
      {
        level: 1,
        delay: 0,
        targets: [
          { type: "pagerduty", service_key: "oncall-business" },
          { type: "email", recipients: ["business-team@company.com"] }
        ],
        auto_resolve: false
      },
      {
        level: 2,
        delay: 120,            // 2分钟后
        targets: [
          { type: "pagerduty", service_key: "business-lead" },
          { type: "email", recipients: ["business-lead@company.com", "executives@company.com"] },
          { type: "slack", channel: "#executive-alerts" }
        ],
        auto_resolve: false
      }
    ],
    max_escalation_level: 2,
    repeat_interval: 900       // 15分钟重复通知
  })
  
  // 创建告警恢复管理器
  let recovery_manager = AlertRecoveryManager::new()
  
  // 配置恢复策略
  RecoveryManager::add_policy(recovery_manager, "auto_recovery", {
    name: "Auto Recovery Policy",
    description: "Automatic recovery for transient issues",
    conditions: [
      { field: "severity", operator: "!=", value: "critical" },
      { field: "auto_recovery_enabled", operator: "=", value: "true" }
    ],
    recovery_actions: [
      { type: "restart_service", delay: 60 },
      { type: "clear_cache", delay: 30 },
      { type: "scale_up", delay: 120, condition: "resource_exhaustion" }
    ],
    max_recovery_attempts: 3,
    recovery_timeout: 600
  })
  
  // 创建测试告警
  let critical_alert = {
    id: "alert-critical-001",
    rule_name: "service_down",
    service: "payment.service",
    severity: "critical",
    business_impact: "high",
    timestamp: 1640995200,
    acknowledged: false,
    resolved: false,
    escalation_level: 0,
    escalation_history: []
  }
  
  let warning_alert = {
    id: "alert-warning-001",
    rule_name: "high_memory_usage",
    service: "cache.service",
    severity: "warning",
    business_impact: "medium",
    timestamp: 1640995200,
    acknowledged: false,
    resolved: false,
    escalation_level: 0,
    escalation_history: [],
    auto_recovery_enabled: "true"
  }
  
  // 测试告警升级
  let escalation_result = EscalationManager::process_alert(escalation_manager, critical_alert)
  
  // 验证初始升级
  assert_eq(escalation_result.current_level, 1)
  assert_eq(escalation_result.escalated, true)
  assert_eq(escalation_result.notifications_sent, 2)  // pagerduty + slack
  assert_true(escalation_result.next_escalation_time > 0)
  
  // 验证升级历史
  assert_eq(escalation_result.escalation_history.length(), 1)
  let first_escalation = escalation_result.escalation_history[0]
  assert_eq(first_escalation.level, 1)
  assert_eq(first_escalation.policy, "critical_service")
  assert_true(first_escalation.timestamp > 0)
  
  // 模拟时间推进，触发二级升级
  let future_time = 1640995200 + 400  // 6分40秒后
  let level2_result = EscalationManager::check_escalation(escalation_manager, critical_alert, future_time)
  
  // 验证二级升级
  assert_eq(level2_result.current_level, 2)
  assert_eq(level2_result.escalated, true)
  assert_eq(level2_result.notifications_sent, 3)  // pagerduty + email + slack
  assert_eq(level2_result.escalation_history.length(), 2)
  
  // 模拟时间推进，触发三级升级
  let future_time2 = 1640995200 + 1000  // 16分40秒后
  let level3_result = EscalationManager::check_escalation(escalation_manager, critical_alert, future_time2)
  
  // 验证三级升级
  assert_eq(level3_result.current_level, 3)
  assert_eq(level3_result.escalated, true)
  assert_eq(level3_result.notifications_sent, 4)  // pagerduty + email + slack + phone
  assert_eq(level3_result.escalation_history.length(), 3)
  
  // 验证最大升级级别
  assert_eq(level3_result.max_level_reached, true)
  assert_eq(level3_result.next_escalation_time, 0)  // 不再升级
  
  // 测试告警确认
  let acknowledged_alert = EscalationManager::acknowledge_alert(escalation_manager, critical_alert, "oncall-engineer")
  
  // 验证告警确认
  assert_eq(acknowledged_alert.acknowledged, true)
  assert_true(acknowledged_alert.acknowledged_by == "oncall-engineer")
  assert_true(acknowledged_alert.acknowledged_at > 0)
  assert_true(acknowledged_alert.escalation_paused)  // 确认后暂停升级
  
  // 测试告警恢复
  let recovery_result = RecoveryManager::process_recovery(recovery_manager, warning_alert)
  
  // 验证恢复处理
  assert_eq(recovery_result.recovery_initiated, true)
  assert_eq(recovery_result.recovery_actions.length(), 3)  // restart_service + clear_cache + scale_up
  
  // 验证恢复动作
  let restart_action = recovery_result.recovery_actions.find(fn(a) { a.type == "restart_service" })
  assert_true(restart_action != None)
  
  match restart_action {
    Some(action) => {
      assert_eq(action.delay, 60)
      assert_eq(action.status, "pending")
    }
    None => assert_true(false)
  }
  
  // 模拟恢复动作执行
  let recovery_execution = RecoveryManager::execute_recovery_actions(recovery_manager, recovery_result.recovery_actions)
  
  // 验证恢复执行
  assert_eq(recovery_execution.total_actions, 3)
  assert_eq(recovery_execution.successful_actions, 3)
  assert_eq(recovery_execution.failed_actions, 0)
  
  // 验证告警自动恢复
  let recovered_alert = RecoveryManager::mark_recovered(recovery_manager, warning_alert, "auto_recovery")
  
  // 验证恢复状态
  assert_eq(recovered_alert.resolved, true)
  assert_eq(recovered_alert.resolution_method, "auto_recovery")
  assert_true(recovered_alert.resolved_at > 0)
  assert_true(recovered_alert.resolution_notes.length() > 0)
  
  // 测试升级后的告警恢复
  let critical_recovery = EscalationManager::resolve_alert(escalation_manager, critical_alert, "manual", "Issue fixed by restarting database connection pool")
  
  // 验证关键告警恢复
  assert_eq(critical_recovery.resolved, true)
  assert_eq(critical_recovery.resolution_method, "manual")
  assert_true(critical_recovery.resolved_at > 0)
  assert_true(critical_recovery.resolution_notes.contains("database"))
  
  // 验证升级通知
  assert_true(critical_recovery.recovery_notifications_sent > 0)
  assert_true(critical_recovery.escalation_stopped)
  
  // 测试恢复通知
  let recovery_notifications = EscalationManager::send_recovery_notifications(escalation_manager, critical_recovery)
  
  // 验证恢复通知
  assert_eq(recovery_notifications.total_notifications, critical_recovery.escalation_level)
  assert_eq(recovery_notifications.successful_notifications, critical_recovery.escalation_level)
  assert_true(recovery_notifications.notifications.any(fn(n) { n.type == "email" and n.subject.contains("resolved") }))
}

// 测试5: 告警性能和负载测试
test "告警系统性能和负载测试" {
  // 创建告警性能测试器
  let alert_performance_tester = AlertPerformanceTester::new()
  
  // 配置性能测试参数
  AlertPerformanceTester::set_test_parameters(alert_performance_tester, {
    alert_rate: 100,         // 每秒100个告警
    test_duration: 60,       // 测试60秒
    concurrent_evaluators: 4,
    batch_size: 10,
    max_latency_ms: 100      // 最大延迟100ms
  })
  
  // 创建大量告警规则
  let rule_count = 50
  for i in 0..=rule_count - 1 {
    AlertPerformanceTester::add_rule(alert_performance_tester, "performance-rule-" + i.to_string(), {
      name: "Performance Rule " + i.to_string(),
      condition: "metric_" + i.to_string() + " > " + (50 + i).to_string(),
      severity: if i % 3 == 0 { "critical" } else if i % 3 == 1 { "warning" } else { "info" },
      evaluation_interval: 10 + i % 20,
      for_duration: i % 60
    })
  }
  
  // 生成高负载数据流
  let high_load_data_stream = AlertPerformanceTester::generate_high_load_stream(alert_performance_tester, {
    metrics_per_alert: 10,
    variance_factor: 0.2,
    burst_probability: 0.1,
    burst_size_multiplier: 5.0
  })
  
  // 执行性能测试
  let performance_result = AlertPerformanceTester::run_performance_test(alert_performance_tester, high_load_data_stream)
  
  // 验证性能测试结果
  assert_true(performance_result.test_completed)
  assert_true(performance_result.total_alerts_processed > 0)
  assert_eq(performance_result.test_duration, 60)
  
  // 验证吞吐量
  assert_true(performance_result.average_throughput >= 90)  // 至少90%的预期吞吐量
  assert_true(performance_result.peak_throughput >= performance_result.average_throughput)
  
  // 验证延迟
  assert_true(performance_result.average_latency_ms <= 100)  // 平均延迟不超过100ms
  assert_true(performance_result.p95_latency_ms <= 200)     // P95延迟不超过200ms
  assert_true(performance_result.p99_latency_ms <= 500)     // P99延迟不超过500ms
  
  // 验证错误率
  assert_true(performance_result.error_rate <= 0.01)        // 错误率不超过1%
  assert_eq(performance_result.timeouts, 0)                 // 不应该有超时
  
  // 验证资源使用
  assert_true(performance_result.max_cpu_usage <= 80.0)     // CPU使用率不超过80%
  assert_true(performance_result.max_memory_usage <= 512.0) // 内存使用不超过512MB
  
  // 执行并发测试
  let concurrent_result = AlertPerformanceTester::run_concurrent_test(alert_performance_tester, {
    concurrent_threads: 8,
    alerts_per_thread: 100,
    shared_rules: true,
    synchronization_required: true
  })
  
  // 验证并发测试结果
  assert_eq(concurrent_result.total_threads, 8)
  assert_eq(concurrent_result.total_alerts_processed, 800)
  assert_eq(concurrent_result.concurrent_errors, 0)
  
  // 验证线程安全性
  assert_true(concurrent_result.data_race_detected == false)
  assert_true(concurrent_result.deadlock_detected == false)
  
  // 执行内存压力测试
  let memory_stress_result = AlertPerformanceTester::run_memory_stress_test(alert_performance_tester, {
    initial_alert_count: 1000,
    growth_rate: 100,         // 每秒增加100个告警
    max_alert_count: 10000,
    gc_pressure: true
  })
  
  // 验证内存压力测试
  assert_true(memory_stress_result.max_alerts_reached >= 5000)
  assert_true(memory_stress_result.memory_growth_rate > 0)
  assert_true(memory_stress_result.gc_efficiency > 0.8)  // GC效率超过80%
  
  // 执行长时间稳定性测试
  let stability_result = AlertPerformanceTester::run_stability_test(alert_performance_tester, {
    test_duration: 3600,      // 1小时
    alert_rate: 50,           // 每秒50个告警
    resource_monitoring: true,
    memory_leak_detection: true
  })
  
  // 验证稳定性测试结果
  assert_true(stability_result.test_completed)
  assert_true(stability_result.memory_leak_detected == false)
  assert_true(stability_result.performance_degradation < 0.1)  // 性能下降不超过10%
  
  // 验证系统稳定性指标
  assert_true(stability_result.average_memory_usage_stable)
  assert_true(stability_result.average_cpu_usage_stable)
  assert_true(stability_result.no_critical_errors)
  
  // 生成性能报告
  let performance_report = AlertPerformanceTester::generate_performance_report(alert_performance_tester, [
    performance_result,
    concurrent_result,
    memory_stress_result,
    stability_result
  ])
  
  // 验证性能报告
  assert_true(performance_report.executive_summary.length() > 0)
  assert_true(performance_report.throughput_analysis.length() > 0)
  assert_true(performance_report.latency_analysis.length() > 0)
  assert_true(performance_report.resource_usage_analysis.length() > 0)
  assert_true(performance_report.recommendations.length() > 0)
  
  // 验证建议内容
  let performance_recommendations = performance_report.recommendations.filter(fn(r) { r.category == "performance" })
  assert_true(performance_recommendations.length() > 0)
  
  let scalability_recommendations = performance_report.recommendations.filter(fn(r) { r.category == "scalability" })
  assert_true(scalability_recommendations.length() > 0)
  
  // 验证基准比较
  let benchmark_comparison = AlertPerformanceTester::compare_with_benchmarks(alert_performance_tester, performance_result)
  
  // 验证基准比较
  assert_true(benchmark_comparison.benchmark_name.length() > 0)
  assert_true(benchmark_comparison.comparison_metrics.length() > 0)
  
  let throughput_comparison = benchmark_comparison.comparison_metrics.find(fn(m) { m.metric == "throughput" })
  assert_true(throughput_comparison != None)
  
  match throughput_comparison {
    Some(comp) => {
      assert_true(comp.current_value > 0)
      assert_true(comp.benchmark_value > 0)
      assert_true(comp.performance_ratio > 0)
    }
    None => assert_true(false)
  }
}