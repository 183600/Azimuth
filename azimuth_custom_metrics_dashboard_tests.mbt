// Azimuth Custom Metrics Dashboard Tests
// 自定义度量仪表盘测试用例 - 专注于度量收集、仪表盘配置和可视化数据

// Test 1: 自定义度量仪表盘配置
test "custom metrics dashboard configuration" {
  // 创建仪表盘配置管理器
  let dashboard_manager = DashboardManager::new()
  
  // 创建自定义仪表盘
  let dashboard = Dashboard::new("system_overview", "System Overview Dashboard")
  Dashboard::set_description(dashboard, "Comprehensive system performance and health monitoring")
  Dashboard::set_refresh_interval(dashboard, 30) // 30秒刷新间隔
  Dashboard::set_time_range(dashboard, TimeRange::LastHour)
  
  // 添加面板配置
  let cpu_panel = Panel::new("cpu_usage", "CPU Usage", PanelType::Gauge)
  Panel::set_metric_query(cpu_panel, "avg by (instance) (100 - (avg by (instance) (irate(node_cpu_seconds_total{mode='idle'}[5m])) * 100))")
  Panel::set_unit(cpu_panel, "percent")
  Panel::set_min(cpu_panel, 0.0)
  Panel::set_max(cpu_panel, 100.0)
  Panel::add_threshold(cpu_panel, Threshold::new(80.0, "warning", "yellow"))
  Panel::add_threshold(cpu_panel, Threshold::new(95.0, "critical", "red"))
  Dashboard::add_panel(dashboard, cpu_panel)
  
  let memory_panel = Panel::new("memory_usage", "Memory Usage", PanelType::Gauge)
  Panel::set_metric_query(memory_panel, "(1 - (node_memory_MemAvailable_bytes / node_memory_MemTotal_bytes)) * 100")
  Panel::set_unit(memory_panel, "percent")
  Panel::set_min(memory_panel, 0.0)
  Panel::set_max(memory_panel, 100.0)
  Panel::add_threshold(memory_panel, Threshold::new(85.0, "warning", "yellow"))
  Panel::add_threshold(memory_panel, Threshold::new(95.0, "critical", "red"))
  Dashboard::add_panel(dashboard, memory_panel)
  
  let request_rate_panel = Panel::new("request_rate", "Request Rate", PanelType::TimeSeries)
  Panel::set_metric_query(request_rate_panel, "sum by (service) (rate(http_requests_total[5m]))")
  Panel::set_unit(request_rate_panel, "reqps")
  Dashboard::add_panel(dashboard, request_rate_panel)
  
  let error_rate_panel = Panel::new("error_rate", "Error Rate", PanelType::TimeSeries)
  Panel::set_metric_query(error_rate_panel, "sum by (service) (rate(http_requests_total{status=~'5..'}[5m]))")
  Panel::set_unit(error_rate_panel, "reqps")
  Panel::add_threshold(error_rate_panel, Threshold::new(10.0, "warning", "yellow"))
  Panel::add_threshold(error_rate_panel, Threshold::new(50.0, "critical", "red"))
  Dashboard::add_panel(dashboard, error_rate_panel)
  
  let response_time_panel = Panel::new("response_time", "Response Time", PanelType::Heatmap)
  Panel::set_metric_query(response_time_panel, "sum by (le) (rate(http_request_duration_seconds_bucket[5m]))")
  Panel::set_unit(response_time_panel, "seconds")
  Dashboard::add_panel(dashboard, response_time_panel)
  
  // 保存仪表盘配置
  let save_result = DashboardManager::save_dashboard(dashboard_manager, dashboard)
  match save_result {
    Ok(dashboard_id) => {
      assert_true(dashboard_id.length() > 0)
      
      // 验证仪表盘加载
      let loaded_dashboard = DashboardManager::load_dashboard(dashboard_manager, dashboard_id)
      match loaded_dashboard {
        Some(dash) => {
          assert_eq(dash.name, "system_overview")
          assert_eq(dash.display_name, "System Overview Dashboard")
          assert_eq(dash.panels.length(), 5)
          assert_eq(dash.refresh_interval, 30)
        }
        None => assert_true(false)
      }
    }
    Err(_) => assert_true(false)
  }
}

// Test 2: 实时度量数据收集和更新
test "real-time metrics collection and updates" {
  // 创建度量提供者
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "dashboard_metrics")
  
  // 创建各种类型的度量
  let cpu_gauge = Meter::create_gauge(meter, "cpu_usage", Some("CPU Usage"), Some("percent"))
  let memory_gauge = Meter::create_gauge(meter, "memory_usage", Some("Memory Usage"), Some("percent"))
  let request_counter = Meter::create_counter(meter, "requests_total", Some("Total Requests"), Some("requests"))
  let error_counter = Meter::create_counter(meter, "errors_total", Some("Total Errors"), Some("errors"))
  let response_time_histogram = Meter::create_histogram(meter, "response_time_seconds", Some("Response Time"), Some("seconds"))
  
  // 创建实时数据收集器
  let collector = RealtimeCollector::new(provider)
  Collector::add_metric(collector, cpu_gauge)
  Collector::add_metric(collector, memory_gauge)
  Collector::add_metric(collector, request_counter)
  Collector::add_metric(collector, error_counter)
  Collector::add_metric(collector, response_time_histogram)
  
  // 模拟实时数据更新
  let services = ["web", "api", "auth", "payment"]
  let instances = ["web-01", "web-02", "api-01", "api-02", "auth-01", "payment-01"]
  
  for i in 0..<100 {
    for service in services {
      for instance in instances {
        // 设置CPU和内存使用率
        let cpu_usage = 50.0 + (i % 50).to_float() + Math::random() * 10.0
        let memory_usage = 60.0 + (i % 40).to_float() + Math::random() * 15.0
        
        let cpu_attrs = Attributes::new()
        Attributes::set(cpu_attrs, "service", StringValue(service))
        Attributes::set(cpu_attrs, "instance", StringValue(instance))
        Gauge::set(cpu_gauge, cpu_usage, Some(cpu_attrs))
        
        let memory_attrs = Attributes::new()
        Attributes::set(memory_attrs, "service", StringValue(service))
        Attributes::set(memory_attrs, "instance", StringValue(instance))
        Gauge::set(memory_gauge, memory_usage, Some(memory_attrs))
        
        // 记录请求和错误
        let request_attrs = Attributes::new()
        Attributes::set(request_attrs, "service", StringValue(service))
        Attributes::set(request_attrs, "status", StringValue("200"))
        Counter::add(request_counter, 1.0, Some(request_attrs))
        
        // 偶尔记录错误
        if i % 10 == 0 {
          let error_attrs = Attributes::new()
          Attributes::set(error_attrs, "service", StringValue(service))
          Attributes::set(error_attrs, "status", StringValue("500"))
          Counter::add(error_counter, 1.0, Some(error_attrs))
        }
        
        // 记录响应时间
        let response_time = 0.1 + Math::random() * 2.0
        let response_attrs = Attributes::new()
        Attributes::set(response_attrs, "service", StringValue(service))
        Histogram::record(response_time_histogram, response_time, Some(response_attrs))
      }
    }
    
    // 每10次迭代收集一次数据
    if i % 10 == 0 {
      let snapshot = Collector::collect_snapshot(collector)
      assert_true(snapshot.metrics.length() > 0)
      assert_true(snapshot.timestamp > 0)
    }
  }
  
  // 测试实时数据查询
  let query_engine = MetricsQueryEngine::new()
  QueryEngine::register_metrics(query_engine, collector)
  
  // 查询CPU使用率
  let cpu_query = "avg by (service) (cpu_usage)"
  let cpu_result = QueryEngine::execute(query_engine, cpu_query)
  match cpu_result {
    Ok(results) => {
      assert_true(results.length() > 0)
      for result in results {
        assert_true(result.metric.contains("service"))
        assert_true(result.value >= 0.0 && result.value <= 100.0)
      }
    }
    Err(_) => assert_true(false)
  }
  
  // 查询错误率
  let error_rate_query = "sum by (service) (errors_total) / sum by (service) (requests_total) * 100"
  let error_rate_result = QueryEngine::execute(query_engine, error_rate_query)
  match error_rate_result {
    Ok(results) => {
      assert_true(results.length() > 0)
      for result in results {
        assert_true(result.metric.contains("service"))
        assert_true(result.value >= 0.0 && result.value <= 100.0)
      }
    }
    Err(_) => assert_true(false)
  }
}

// Test 3: 仪表盘模板和变量
test "dashboard templates and variables" {
  // 创建仪表盘模板管理器
  let template_manager = TemplateManager::new()
  
  // 创建仪表盘模板
  let service_dashboard_template = DashboardTemplate::new("service_overview", "Service Overview")
  
  // 添加模板变量
  let service_var = TemplateVariable::new("service", "Service")
  TemplateVariable::set_type(service_var, VariableType::Query)
  TemplateVariable::set_query(service_var, "label_values(service)")
  TemplateVariable::set_multi(service_var, true)
  TemplateVariable::set_all_option(service_var, true)
  TemplateVariable::set_default_value(service_var, ["web", "api"])
  TemplateManager::add_variable(service_dashboard_template, service_var)
  
  let instance_var = TemplateVariable::new("instance", "Instance")
  TemplateVariable::set_type(instance_var, VariableType::Query)
  TemplateVariable::set_query(instance_var, "label_values(instance, service=~\"$service\")")
  TemplateVariable::set_multi(instance_var, true)
  TemplateVariable::set_all_option(instance_var, true)
  TemplateManager::add_variable(service_dashboard_template, instance_var)
  
  let time_range_var = TemplateVariable::new("time_range", "Time Range")
  TemplateVariable::set_type(time_range_var, VariableType::Custom)
  TemplateVariable::set_options(time_range_var, [
    VariableOption::new("1h", "Last 1 Hour"),
    VariableOption::new("6h", "Last 6 Hours"),
    VariableOption::new("24h", "Last 24 Hours"),
    VariableOption::new("7d", "Last 7 Days")
  ])
  TemplateVariable::set_default_value(time_range_var, ["6h"])
  TemplateManager::add_variable(service_dashboard_template, time_range_var)
  
  // 添加使用模板变量的面板
  let request_rate_panel = Panel::new("service_request_rate", "Service Request Rate", PanelType::TimeSeries)
  Panel::set_metric_query(request_rate_panel, "sum by (instance) (rate(requests_total{service=~\"$service\", instance=~\"$instance\"}[$time_range]))")
  TemplateManager::add_panel(service_dashboard_template, request_rate_panel)
  
  let error_rate_panel = Panel::new("service_error_rate", "Service Error Rate", PanelType::TimeSeries)
  Panel::set_metric_query(error_rate_panel, "sum by (instance) (rate(requests_total{service=~\"$service\", instance=~\"$instance\", status=~\"5..\"}[$time_range]))")
  TemplateManager::add_panel(service_dashboard_template, error_rate_panel)
  
  let response_time_panel = Panel::new("service_response_time", "Service Response Time", PanelType::TimeSeries)
  Panel::set_metric_query(response_time_panel, "histogram_quantile(0.95, sum by (le, instance) (rate(response_time_seconds_bucket{service=~\"$service\", instance=~\"$instance\"}[$time_range])))")
  TemplateManager::add_panel(service_dashboard_template, response_time_panel)
  
  // 保存模板
  let save_result = TemplateManager::save_template(template_manager, service_dashboard_template)
  match save_result {
    Ok(template_id) => {
      // 测试模板实例化
      let variables = [
        ("service", ["web", "api"]),
        ("instance", ["web-01", "api-01"]),
        ("time_range", ["6h"])
      ]
      
      let instantiated_dashboard = TemplateManager::instantiate(template_manager, template_id, variables)
      match instantiated_dashboard {
        Some(dashboard) => {
          assert_eq(dashboard.panels.length(), 3)
          
          // 验证变量替换
          let request_rate_query = dashboard.panels[0].metric_query
          assert_true(request_rate_query.contains("service=~\"web|api\""))
          assert_true(request_rate_query.contains("instance=~\"web-01|api-01\""))
          assert_true(request_rate_query.contains("[6h]"))
          
          let error_rate_query = dashboard.panels[1].metric_query
          assert_true(error_rate_query.contains("service=~\"web|api\""))
          assert_true(error_rate_query.contains("instance=~\"web-01|api-01\""))
          assert_true(error_rate_query.contains("[6h]"))
          
          let response_time_query = dashboard.panels[2].metric_query
          assert_true(response_time_query.contains("service=~\"web|api\""))
          assert_true(response_time_query.contains("instance=~\"web-01|api-01\""))
          assert_true(response_time_query.contains("[6h]"))
        }
        None => assert_true(false)
      }
    }
    Err(_) => assert_true(false)
  }
}

// Test 4: 仪表盘警报和通知
test "dashboard alerts and notifications" {
  // 创建警报管理器
  let alert_manager = AlertManager::new()
  
  // 创建警报规则
  let high_cpu_alert = AlertRule::new("high_cpu_usage", "High CPU Usage")
  AlertRule::set_query(high_cpu_alert, "avg by (instance) (100 - (avg by (instance) (irate(node_cpu_seconds_total{mode='idle'}[5m])) * 100))")
  AlertRule::set_condition(high_cpu_alert, AlertCondition::GreaterThan(80.0))
  AlertRule::set_duration(high_cpu_alert, 300) // 5分钟
  AlertRule::set_severity(high_cpu_alert, AlertSeverity::Warning)
  AlertRule::set_message(high_cpu_alert, "CPU usage is above 80% for instance {{ $labels.instance }}")
  AlertRule::add_annotation(high_cpu_alert, "summary", "High CPU usage detected")
  AlertRule::add_annotation(high_cpu_alert, "description", "CPU usage has been above 80% for more than 5 minutes")
  AlertManager::add_rule(alert_manager, high_cpu_alert)
  
  let critical_memory_alert = AlertRule::new("critical_memory_usage", "Critical Memory Usage")
  AlertRule::set_query(critical_memory_alert, "(1 - (node_memory_MemAvailable_bytes / node_memory_MemTotal_bytes)) * 100")
  AlertRule::set_condition(critical_memory_alert, AlertCondition::GreaterThan(95.0))
  AlertRule::set_duration(critical_memory_alert, 120) // 2分钟
  AlertRule::set_severity(critical_memory_alert, AlertSeverity::Critical)
  AlertRule::set_message(critical_memory_alert, "Memory usage is critically high at {{ $value }}%")
  AlertRule::add_annotation(critical_memory_alert, "summary", "Critical memory usage")
  AlertRule::add_annotation(critical_memory_alert, "description", "Memory usage is above 95% and requires immediate attention")
  AlertManager::add_rule(alert_manager, critical_memory_alert)
  
  let high_error_rate_alert = AlertRule::new("high_error_rate", "High Error Rate")
  AlertRule::set_query(high_error_rate_alert, "sum by (service) (rate(requests_total{status=~\"5..\"}[5m])) / sum by (service) (rate(requests_total[5m])) * 100")
  AlertRule::set_condition(high_error_rate_alert, AlertCondition::GreaterThan(10.0))
  AlertRule::set_duration(high_error_rate_alert, 180) // 3分钟
  AlertRule::set_severity(high_error_rate_alert, AlertSeverity::Warning)
  AlertRule::set_message(high_error_rate_alert, "Error rate is {{ $value }}% for service {{ $labels.service }}")
  AlertRule::add_annotation(high_error_rate_alert, "summary", "High error rate detected")
  AlertRule::add_annotation(high_error_rate_alert, "description", "Error rate is above 10% for more than 3 minutes")
  AlertManager::add_rule(alert_manager, high_error_rate_alert)
  
  // 创建通知渠道
  let email_channel = NotificationChannel::new("email", "Email Notifications")
  NotificationChannel::set_type(email_channel, ChannelType::Email)
  NotificationChannel::add_recipient(email_channel, "ops-team@example.com")
  NotificationChannel::add_recipient(email_channel, "dev-team@example.com")
  NotificationChannel::set_enabled(email_channel, true)
  AlertManager::add_notification_channel(alert_manager, email_channel)
  
  let slack_channel = NotificationChannel::new("slack", "Slack Notifications")
  NotificationChannel::set_type(slack_channel, ChannelType::Slack)
  NotificationChannel::set_webhook_url(slack_channel, "https://hooks.slack.com/services/...")
  NotificationChannel::set_channel(slack_channel, "#alerts")
  NotificationChannel::set_enabled(slack_channel, true)
  AlertManager::add_notification_channel(alert_manager, slack_channel)
  
  // 配置警报路由
  let critical_route = AlertRoute::new("critical", "Critical Alerts")
  AlertRoute::add_condition(critical_route, RouteCondition::SeverityEquals(AlertSeverity::Critical))
  AlertRoute::add_receiver(critical_route, "email")
  AlertRoute::add_receiver(critical_route, "slack")
  AlertRoute::set_group_by(critical_route, ["alertname", "instance"])
  AlertRoute::set_group_wait(critical_route, 30)
  AlertRoute::set_group_interval(critical_route, 300)
  AlertRoute::set_repeat_interval(critical_route, 3600)
  AlertManager::add_route(alert_manager, critical_route)
  
  let warning_route = AlertRoute::new("warning", "Warning Alerts")
  AlertRoute::add_condition(warning_route, RouteCondition::SeverityEquals(AlertSeverity::Warning))
  AlertRoute::add_receiver(warning_route, "email")
  AlertRoute::set_group_by(warning_route, ["alertname"])
  AlertRoute::set_group_wait(warning_route, 60)
  AlertRoute::set_group_interval(warning_route, 600)
  AlertRoute::set_repeat_interval(warning_route, 7200)
  AlertManager::add_route(alert_manager, warning_route)
  
  // 测试警报评估
  let metrics_provider = MockMetricsProvider::new()
  
  // 模拟高CPU使用率
  MockMetricsProvider::add_metric(metrics_provider, "cpu_usage", 85.0, [("instance", "web-01")])
  
  // 模拟临界内存使用率
  MockMetricsProvider::add_metric(metrics_provider, "memory_usage", 97.0, [("instance", "db-01")])
  
  // 模拟高错误率
  MockMetricsProvider::add_metric(metrics_provider, "error_rate", 15.0, [("service", "payment")])
  
  // 评估警报规则
  let alert_evaluator = AlertEvaluator::new(alert_manager, metrics_provider)
  let evaluation_result = Evaluator::evaluate_rules(alert_evaluator)
  
  // 验证警报触发
  assert_eq(evaluation_result.active_alerts.length(), 3)
  
  let cpu_alert = EvaluationResult::get_alert_by_name(evaluation_result, "high_cpu_usage")
  match cpu_alert {
    Some(alert) => {
      assert_eq(alert.severity, AlertSeverity::Warning)
      assert_eq(alert.status, AlertStatus::Firing)
      assert_true(alert.message.contains("web-01"))
    }
    None => assert_true(false)
  }
  
  let memory_alert = EvaluationResult::get_alert_by_name(evaluation_result, "critical_memory_usage")
  match memory_alert {
    Some(alert) => {
      assert_eq(alert.severity, AlertSeverity::Critical)
      assert_eq(alert.status, AlertStatus::Firing)
      assert_true(alert.message.contains("97.0"))
    }
    None => assert_true(false)
  }
  
  let error_rate_alert = EvaluationResult::get_alert_by_name(evaluation_result, "high_error_rate")
  match error_rate_alert {
    Some(alert) => {
      assert_eq(alert.severity, AlertSeverity::Warning)
      assert_eq(alert.status, AlertStatus::Firing)
      assert_true(alert.message.contains("payment"))
    }
    None => assert_true(false)
  }
  
  // 测试通知发送
  let notification_sender = NotificationSender::new(alert_manager)
  let sent_notifications = Sender::send_notifications(notification_sender, evaluation_result.active_alerts)
  
  // 验证通知发送
  assert_true(sent_notifications.length() > 0)
  
  // 临界警报应该发送到两个渠道
  let critical_notifications = NotificationList::filter_by_alert(sent_notifications, "critical_memory_usage")
  assert_eq(critical_notifications.length(), 2) // email + slack
  
  // 警告警报应该只发送到邮件
  let warning_notifications = NotificationList::filter_by_alert(sent_notifications, "high_cpu_usage")
  assert_eq(warning_notifications.length(), 1) // email only
}