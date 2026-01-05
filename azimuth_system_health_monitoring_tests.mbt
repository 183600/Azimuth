// Azimuth 系统健康监控测试
// 专注于系统健康检查、监控指标和自动恢复机制

// 测试10: 系统健康检查测试
test "系统健康检查测试" {
  // 创建系统健康监控器
  let health_monitor = SystemHealthMonitor::new()
  
  // 配置健康检查规则
  HealthMonitor::add_check_rule(health_monitor, {
    name: "service_availability",
    type: "availability",
    description: "检查服务可用性",
    target: "service.*",
    interval: 30,  // 30秒检查一次
    timeout: 10,   // 10秒超时
    threshold: {
      success_rate: 0.95,  // 95%成功率
      response_time: 1000  // 1秒响应时间
    },
    action: {
      type: "alert",
      severity: "critical"
    }
  })
  
  HealthMonitor::add_check_rule(health_monitor, {
    name: "resource_usage",
    type: "resource",
    description: "检查资源使用情况",
    target: "system.*",
    interval: 60,  // 1分钟检查一次
    threshold: {
      cpu_usage: 80.0,     // 80% CPU使用率
      memory_usage: 85.0,  // 85%内存使用率
      disk_usage: 90.0     // 90%磁盘使用率
    },
    action: {
      type: "scaling",
      severity: "warning"
    }
  })
  
  HealthMonitor::add_check_rule(health_monitor, {
    name: "error_rate",
    type: "error",
    description: "检查错误率",
    target: "service.*",
    interval: 60,  // 1分钟检查一次
    window: 300,   // 5分钟窗口
    threshold: {
      error_rate: 0.05  // 5%错误率
    },
    action: {
      type: "recovery",
      severity: "critical"
    }
  })
  
  HealthMonitor::add_check_rule(health_monitor, {
    name: "dependency_health",
    type: "dependency",
    description: "检查依赖服务健康",
    target: "dependency.*",
    interval: 30,  // 30秒检查一次
    timeout: 5,    // 5秒超时
    threshold: {
      availability: 0.99,  // 99%可用性
      response_time: 500   // 500ms响应时间
    },
    action: {
      type: "circuit_breaker",
      severity: "warning"
    }
  })
  
  // 创建模拟服务
  let services = [
    {
      name: "service.a",
      status: "healthy",
      endpoints: [
        { path: "/health", method: "GET", response_time: 50, success_rate: 0.99 },
        { path: "/api/users", method: "GET", response_time: 150, success_rate: 0.98 }
      ],
      resources: {
        cpu_usage: 45.0,
        memory_usage: 60.0,
        disk_usage: 30.0
      },
      dependencies: ["service.b", "database"]
    },
    {
      name: "service.b",
      status: "healthy",
      endpoints: [
        { path: "/health", method: "GET", response_time: 80, success_rate: 0.97 },
        { path: "/api/orders", method: "POST", response_time: 300, success_rate: 0.95 }
      ],
      resources: {
        cpu_usage: 65.0,
        memory_usage: 70.0,
        disk_usage: 40.0
      },
      dependencies: ["service.c", "cache"]
    },
    {
      name: "service.c",
      status: "degraded",  // 部分故障
      endpoints: [
        { path: "/health", method: "GET", response_time: 200, success_rate: 0.90 },
        { path: "/api/products", method: "GET", response_time: 800, success_rate: 0.85 }
      ],
      resources: {
        cpu_usage: 85.0,  // 高CPU使用率
        memory_usage: 75.0,
        disk_usage: 50.0
      },
      dependencies: ["database"]
    },
    {
      name: "database",
      status: "healthy",
      endpoints: [
        { path: "/health", method: "GET", response_time: 30, success_rate: 0.99 },
        { path: "/query", method: "POST", response_time: 100, success_rate: 0.99 }
      ],
      resources: {
        cpu_usage: 55.0,
        memory_usage: 80.0,
        disk_usage: 60.0
      },
      dependencies: []
    }
  ]
  
  // 注册服务到健康监控器
  for service in services {
    HealthMonitor::register_service(health_monitor, service)
  }
  
  // 启动健康监控
  HealthMonitor::start(health_monitor)
  
  // 等待初始健康检查完成
  Time::sleep(2000)
  
  // 获取初始健康状态
  let initial_health_status = HealthMonitor::get_overall_health(health_monitor)
  
  // 验证初始健康状态
  assert_true(initial_health_status.overall_status == "healthy" or 
             initial_health_status.overall_status == "degraded")
  assert_true(initial_health_status.checked_services > 0)
  assert_true(initial_health_status.last_check_time > 0)
  
  // 验证各服务的健康状态
  let service_a_health = HealthMonitor::get_service_health(health_monitor, "service.a")
  assert_eq(service_a_health.status, "healthy")
  assert_true(service_a_health.availability >= 0.95)
  
  let service_c_health = HealthMonitor::get_service_health(health_monitor, "service.c")
  assert_eq(service_c_health.status, "degraded")
  assert_true(service_c_health.resource_issues.contains("high_cpu"))
  
  // 模拟服务故障
  let fault_injection = {
    service_name: "service.b",
    fault_type: "high_latency",
    parameters: {
      response_time: 2000,  // 2秒响应时间
      affected_endpoint: "/api/orders",
      duration: 120         // 持续2分钟
    }
  }
  
  HealthMonitor::inject_fault(health_monitor, fault_injection)
  
  // 等待故障检测
  Time::sleep(35000)  // 等待超过检查间隔
  
  // 获取故障后的健康状态
  let fault_health_status = HealthMonitor::get_overall_health(health_monitor)
  
  // 验证故障检测
  assert_true(fault_health_status.overall_status == "degraded" or 
             fault_health_status.overall_status == "unhealthy")
  
  let service_b_fault_health = HealthMonitor::get_service_health(health_monitor, "service.b")
  assert_true(service_b_fault_health.status == "degraded" or 
             service_b_fault_health.status == "unhealthy")
  assert_true(service_b_fault_health.issues.contains("high_latency"))
  
  // 验证告警生成
  let alerts = HealthMonitor::get_active_alerts(health_monitor)
  assert_true(alerts.length() > 0)
  
  let service_b_alert = alerts.find(fn(alert) {
    alert.service == "service.b" and alert.type == "availability"
  })
  assert_true(service_b_alert != None)
  
  match service_b_alert {
    Some(alert) => {
      assert_eq(alert.severity, "critical")
      assert_true(alert.message.contains("response time"))
      assert_true(alert.triggered_time > 0)
    }
    None => assert_true(false)
  }
  
  // 测试自动恢复机制
  let recovery_actions = HealthMonitor::get_recovery_actions(health_monitor)
  assert_true(recovery_actions.length() > 0)
  
  // 执行恢复动作
  for action in recovery_actions {
    let recovery_result = HealthMonitor::execute_recovery_action(health_monitor, action.id)
    assert_true(recovery_result.success)
  }
  
  // 移除故障注入
  HealthMonitor::remove_fault(health_monitor, "service.b")
  
  // 等待恢复检测
  Time::sleep(35000)
  
  // 获取恢复后的健康状态
  let recovery_health_status = HealthMonitor::get_overall_health(health_monitor)
  
  // 验证恢复效果
  assert_true(recovery_health_status.overall_status == "healthy" or 
             recovery_health_status.overall_status == "degraded")
  
  let service_b_recovery_health = HealthMonitor::get_service_health(health_monitor, "service.b")
  assert_true(service_b_recovery_health.status == "healthy" or 
             service_b_recovery_health.status == "degraded")
  
  // 验证告警解决
  let resolved_alerts = HealthMonitor::get_resolved_alerts(health_monitor)
  assert_true(resolved_alerts.length() > 0)
  
  let service_b_resolved_alert = resolved_alerts.find(fn(alert) {
    alert.service == "service.b" and alert.type == "availability"
  })
  assert_true(service_b_resolved_alert != None)
  
  // 测试系统资源监控
  let resource_metrics = HealthMonitor::get_resource_metrics(health_monitor)
  
  // 验证资源指标收集
  assert_true(resource_metrics.length() > 0)
  
  for metric in resource_metrics {
    assert_true(metric.service_name != "")
    assert_true(metric.timestamp > 0)
    assert_true(metric.cpu_usage >= 0.0 and metric.cpu_usage <= 100.0)
    assert_true(metric.memory_usage >= 0.0 and metric.memory_usage <= 100.0)
    assert_true(metric.disk_usage >= 0.0 and metric.disk_usage <= 100.0)
  }
  
  // 测试健康趋势分析
  let trend_analysis = HealthMonitor::analyze_health_trends(health_monitor, {
    time_window: 3600,  // 1小时窗口
    granularity: 300    // 5分钟粒度
  })
  
  // 验证趋势分析结果
  assert_true(trend_analysis.trends.length() > 0)
  
  let service_a_trend = trend_analysis.trends.find(fn(trend) {
    trend.service == "service.a"
  })
  assert_true(service_a_trend != None)
  
  match service_a_trend {
    Some(trend) => {
      assert_true(trend.data_points.length() > 0)
      assert_true(trend.trend_direction == "stable" or 
                 trend.trend.direction == "improving" or 
                 trend.trend_direction == "degrading")
    }
    None => assert_true(false)
  }
  
  // 测试健康报告生成
  let health_report = HealthMonitor::generate_health_report(health_monitor, {
    include_services: true,
    include_dependencies: true,
    include_trends: true,
    include_recommendations: true
  })
  
  // 验证健康报告
  assert_true(health_report.overall_score >= 0.0 and health_report.overall_score <= 100.0)
  assert_true(health_report.service_count > 0)
  assert_true(health_report.generated_at > 0)
  
  // 验证服务健康摘要
  assert_true(health_report.service_summary.contains_key("healthy"))
  assert_true(health_report.service_summary.contains_key("degraded"))
  assert_true(health_report.service_summary.contains_key("unhealthy"))
  
  // 验证依赖关系分析
  assert_true(health_report.dependency_analysis.critical_dependencies.length() >= 0)
  assert_true(health_report.dependency_analysis.affected_services.length() >= 0)
  
  // 验证改进建议
  assert_true(health_report.recommendations.length() > 0)
  
  let has_resource_recommendation = health_report.recommendations.any(fn(rec) {
    rec.category == "resource" and rec.priority == "high"
  })
  assert_true(has_resource_recommendation)
  
  // 测试自定义健康检查
  HealthMonitor::add_custom_check(health_monitor, {
    name: "custom_business_check",
    description: "自定义业务逻辑检查",
    check_function: fn(service_data) {
      // 自定义检查逻辑：检查特定业务指标
      let business_metric = service_data.custom_metrics.get("business_throughput")
      match business_metric {
        Some(value) => value > 100.0,
        None => false
      }
    },
    interval: 120,  // 2分钟检查一次
    threshold: 0.8,
    action: {
      type: "alert",
      severity: "warning"
    }
  })
  
  // 添加自定义指标
  HealthMonitor::add_custom_metric(health_monitor, "service.a", "business_throughput", 150.0)
  HealthMonitor::add_custom_metric(health_monitor, "service.b", "business_throughput", 80.0)  // 低于阈值
  
  // 等待自定义检查执行
  Time::sleep(125000)  // 等待超过自定义检查间隔
  
  // 验证自定义检查结果
  let custom_check_results = HealthMonitor::get_custom_check_results(health_monitor)
  assert_true(custom_check_results.length() > 0)
  
  let service_a_custom_result = custom_check_results.find(fn(result) {
    result.service == "service.a" and result.check_name == "custom_business_check"
  })
  assert_true(service_a_custom_result != None)
  
  match service_a_custom_result {
    Some(result) => {
      assert_true(result.status == "pass")
      assert_true(result.value > 100.0)
    }
    None => assert_true(false)
  }
  
  let service_b_custom_result = custom_check_results.find(fn(result) {
    result.service == "service.b" and result.check_name == "custom_business_check"
  })
  assert_true(service_b_custom_result != None)
  
  match service_b_custom_result {
    Some(result) => {
      assert_true(result.status == "fail")
      assert_true(result.value <= 100.0)
    }
    None => assert_true(false)
  }
  
  // 测试健康检查配置更新
  HealthMonitor::update_check_rule(health_monitor, "resource_usage", {
    threshold: {
      cpu_usage: 75.0,     // 降低CPU阈值
      memory_usage: 80.0,  // 降低内存阈值
      disk_usage: 85.0     // 降低磁盘阈值
    }
  })
  
  // 验证配置更新
  let updated_rule = HealthMonitor::get_check_rule(health_monitor, "resource_usage")
  assert_eq(updated_rule.threshold.cpu_usage, 75.0)
  assert_eq(updated_rule.threshold.memory_usage, 80.0)
  assert_eq(updated_rule.threshold.disk_usage, 85.0)
  
  // 停止健康监控
  HealthMonitor::stop(health_monitor)
  
  // 验证优雅关闭
  let final_status = HealthMonitor::get_monitor_status(health_monitor)
  assert_eq(final_status.status, "stopped")
  assert_true(final_status.stop_time > 0)
}