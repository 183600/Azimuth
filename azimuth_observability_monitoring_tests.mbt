// Azimuth 可观测性和监控测试用例
// 专注于测试系统的可观测性和监控能力

// 测试1: 指标收集和聚合测试
test "指标收集和聚合测试" {
  // 初始化指标提供者
  let meter_provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(meter_provider, "observability.test")
  
  // 创建各种类型的指标
  let request_counter = Meter::create_counter(meter, "http.requests.total")
  let response_size_histogram = Meter::create_histogram(meter, "http.response.size")
  let active_connections_gauge = Meter::create_gauge(meter, "http.connections.active")
  let request_duration_histogram = Meter::create_histogram(meter, "http.request.duration")
  
  // 模拟指标收集
  for i = 0; i < 100; i = i + 1 {
    // 记录请求计数
    Counter::add(request_counter, 1.0, [
      ("method", StringValue("GET")),
      ("status", StringValue("200"))
    ])
    
    // 记录响应大小
    let response_size = (i % 10 + 1) * 100
    Histogram::record(response_size_histogram, response_size.to_float(), [
      ("method", StringValue("GET"))
    ])
    
    // 记录请求持续时间
    let duration = (i % 5 + 1).to_float() * 0.01
    Histogram::record(request_duration_histogram, duration, [
      ("method", StringValue("GET")),
      ("status", StringValue("200"))
    ])
  }
  
  // 更新活跃连接数
  Gauge::set(active_connections_gauge, 25.0)
  
  // 创建指标收集器
  let collector = MetricCollector::new()
  
  // 收集指标
  let collected_metrics = MetricCollector::collect(collector, meter_provider)
  
  // 验证指标收集
  assert_true(collected_metrics.length() > 0)
  
  // 验证计数器指标
  let counter_metric = collected_metrics.find(|metric| metric.name == "http.requests.total")
  assert_true(counter_metric.is_some())
  
  match counter_metric {
    Some(metric) => {
      assert_eq(metric.metric_type, "Counter")
      assert_true(metric.data_points.length() > 0)
      
      let data_point = metric.data_points[0]
      assert_eq(data_point.value, 100.0)
      assert_eq(data_point.attributes.get("method"), Some(StringValue("GET")))
      assert_eq(data_point.attributes.get("status"), Some(StringValue("200")))
    }
    None => assert_true(false)
  }
  
  // 验证仪表指标
  let gauge_metric = collected_metrics.find(|metric| metric.name == "http.connections.active")
  assert_true(gauge_metric.is_some())
  
  match gauge_metric {
    Some(metric) => {
      assert_eq(metric.metric_type, "Gauge")
      assert_true(metric.data_points.length() > 0)
      
      let data_point = metric.data_points[0]
      assert_eq(data_point.value, 25.0)
    }
    None => assert_true(false)
  }
  
  // 验证直方图指标
  let histogram_metric = collected_metrics.find(|metric| metric.name == "http.response.size")
  assert_true(histogram_metric.is_some())
  
  match histogram_metric {
    Some(metric) => {
      assert_eq(metric.metric_type, "Histogram")
      assert_true(metric.data_points.length() > 0)
      
      let data_point = metric.data_points[0]
      assert_eq(data_point.count, 100)
      assert_eq(data_point.sum, 5500.0)  // 100+200+...+1000
    }
    None => assert_true(false)
  }
  
  // 测试指标聚合
  let aggregation = MetricAggregator::new()
  let aggregated_metrics = MetricAggregator::aggregate(aggregation, collected_metrics)
  
  // 验证聚合结果
  assert_true(aggregated_metrics.length() >= collected_metrics.length())
}

// 测试2: 分布式追踪和上下文传播测试
test "分布式追踪和上下文传播测试" {
  // 初始化追踪提供者
  let tracer_provider = TracerProvider::default()
  let tracer = TracerProvider::get_tracer(tracer_provider, "observability.test")
  
  // 创建根span
  let root_span = Tracer::start_span(tracer, "api.request")
  let root_context = Span::context(root_span)
  
  // 设置根span属性
  Span::set_attribute(root_span, "http.method", StringValue("POST"))
  Span::set_attribute(root_span, "http.url", StringValue("https://api.example.com/users"))
  Span::set_attribute(root_span, "user.id", StringValue("user123"))
  
  // 添加事件
  Span::add_event(root_span, "authentication.started", None)
  Span::add_event(root_span, "authentication.completed", Some([("auth.method", StringValue("oauth2"))]))
  
  // 创建子span - 数据库查询
  let db_span = Tracer::start_span_with_context(tracer, "database.query", root_context)
  Span::set_attribute(db_span, "db.statement", StringValue("SELECT * FROM users WHERE id = ?"))
  Span::set_attribute(db_span, "db.type", StringValue("postgresql"))
  
  // 添加数据库事件
  Span::add_event(db_span, "query.executed", None)
  Span::set_status(db_span, Ok)
  Span::end(db_span)
  
  // 创建子span - 缓存操作
  let cache_span = Tracer::start_span_with_context(tracer, "cache.operation", root_context)
  Span::set_attribute(cache_span, "cache.key", StringValue("user:user123"))
  Span::set_attribute(cache_span, "cache.operation", StringValue("get"))
  
  // 添加缓存事件
  Span::add_event(cache_span, "cache.hit", None)
  Span::set_status(cache_span, Ok)
  Span::end(cache_span)
  
  // 创建子span - 外部服务调用
  let external_span = Tracer::start_span_with_context(tracer, "external.service.call", root_context)
  Span::set_attribute(external_span, "http.method", StringValue("GET"))
  Span::set_attribute(external_span, "http.url", StringValue("https://external.api.com/profiles/user123"))
  
  // 添加外部服务事件
  Span::add_event(external_span, "request.sent", None)
  Span::add_event(external_span, "response.received", Some([("http.status_code", IntValue(200))]))
  Span::set_status(external_span, Ok)
  Span::end(external_span)
  
  // 完成根span
  Span::add_event(root_span, "request.completed", None)
  Span::set_status(root_span, Ok)
  Span::end(root_span)
  
  // 创建追踪收集器
  let trace_collector = TraceCollector::new()
  
  // 收集追踪数据
  let collected_traces = TraceCollector::collect(trace_collector, tracer_provider)
  
  // 验证追踪收集
  assert_true(collected_traces.length() > 0)
  
  // 验证根span
  let root_span_data = collected_traces.find(|span| span.name == "api.request")
  assert_true(root_span_data.is_some())
  
  match root_span_data {
    Some(span) => {
      assert_eq(span.name, "api.request")
      assert_eq(span.kind, "Server")
      assert_eq(span.status, "Ok")
      assert_eq(span.parent_span_id, None)
      assert_true(span.events.length() >= 3)
      assert_eq(span.attributes.get("http.method"), Some(StringValue("POST")))
      assert_eq(span.attributes.get("user.id"), Some(StringValue("user123")))
    }
    None => assert_true(false)
  }
  
  // 验证子span的父子关系
  let db_span_data = collected_traces.find(|span| span.name == "database.query")
  let cache_span_data = collected_traces.find(|span| span.name == "cache.operation")
  let external_span_data = collected_traces.find(|span| span.name == "external.service.call")
  
  assert_true(db_span_data.is_some())
  assert_true(cache_span_data.is_some())
  assert_true(external_span_data.is_some())
  
  match (db_span_data, cache_span_data, external_span_data) {
    (Some(db_span), Some(cache_span), Some(external_span)) => {
      // 验证所有子span都有相同的trace_id
      assert_eq(db_span.trace_id, root_span_data.unwrap().trace_id)
      assert_eq(cache_span.trace_id, root_span_data.unwrap().trace_id)
      assert_eq(external_span.trace_id, root_span_data.unwrap().trace_id)
      
      // 验证所有子span都有相同的parent_span_id
      assert_eq(db_span.parent_span_id, Some(root_span_data.unwrap().span_id))
      assert_eq(cache_span.parent_span_id, Some(root_span_data.unwrap().span_id))
      assert_eq(external_span.parent_span_id, Some(root_span_data.unwrap().span_id))
    }
    _ => assert_true(false)
  }
  
  // 测试上下文传播
  let propagator = CompositePropagator::new([W3CTraceContextPropagator::new()])
  let carrier = TextMapCarrier::new()
  
  // 注入上下文
  CompositePropagator::inject(propagator, root_context, carrier)
  
  // 提取上下文
  let extracted_context = CompositePropagator::extract(propagator, carrier)
  
  // 验证传播结果
  let extracted_trace_id = SpanContext::trace_id(extracted_context)
  let extracted_span_id = SpanContext::span_id(extracted_context)
  
  assert_eq(extracted_trace_id, root_span_data.unwrap().trace_id)
  assert_eq(extracted_span_id, root_span_data.unwrap().span_id)
}

// 测试3: 结构化日志记录和关联测试
test "结构化日志记录和关联测试" {
  // 初始化日志提供者
  let logger_provider = LoggerProvider::default()
  let logger = LoggerProvider::get_logger(logger_provider, "observability.test")
  
  // 创建关联的追踪上下文
  let tracer_provider = TracerProvider::default()
  let tracer = TracerProvider::get_tracer(tracer_provider, "observability.test")
  let span = Tracer::start_span(tracer, "logged.operation")
  let span_context = Span::context(span)
  
  // 记录不同级别的日志
  let debug_log = LogRecord::new_with_context(
    Debug,
    Some("Debugging operation details"),
    None,
    Some(Clock::now_unix_nanos(Clock::system())),
    None,
    Some(SpanContext::trace_id(span_context)),
    Some(SpanContext::span_id(span_context)),
    Some(span_context)
  )
  LogRecord::set_attribute(debug_log, "operation.step", IntValue(1))
  LogRecord::set_attribute(debug_log, "operation.detail", StringValue("Validating input parameters"))
  Logger::emit(logger, debug_log)
  
  let info_log = LogRecord::new_with_context(
    Info,
    Some("Operation started successfully"),
    None,
    Some(Clock::now_unix_nanos(Clock::system())),
    None,
    Some(SpanContext::trace_id(span_context)),
    Some(SpanContext::span_id(span_context)),
    Some(span_context)
  )
  LogRecord::set_attribute(info_log, "operation.step", IntValue(2))
  LogRecord::set_attribute(info_log, "operation.id", StringValue("op-12345"))
  Logger::emit(logger, info_log)
  
  let warn_log = LogRecord::new_with_context(
    Warn,
    Some("Operation taking longer than expected"),
    Some("Operation duration exceeded normal threshold"),
    Some(Clock::now_unix_nanos(Clock::system())),
    None,
    Some(SpanContext::trace_id(span_context)),
    Some(SpanContext::span_id(span_context)),
    Some(span_context)
  )
  LogRecord::set_attribute(warn_log, "operation.step", IntValue(3))
  LogRecord::set_attribute(warn_log, "operation.duration_ms", FloatValue(5000.0))
  Logger::emit(logger, warn_log)
  
  let error_log = LogRecord::new_with_context(
    Error,
    Some("Operation failed"),
    Some("Database connection timeout"),
    Some(Clock::now_unix_nanos(Clock::system())),
    None,
    Some(SpanContext::trace_id(span_context)),
    Some(SpanContext::span_id(span_context)),
    Some(span_context)
  )
  LogRecord::set_attribute(error_log, "operation.step", IntValue(4))
  LogRecord::set_attribute(error_log, "error.code", StringValue("DB_TIMEOUT"))
  LogRecord::set_attribute(error_log, "error.retry_count", IntValue(3))
  Logger::emit(logger, error_log)
  
  // 结束span
  Span::set_status(span, Error, Some("Database connection timeout"))
  Span::end(span)
  
  // 创建日志收集器
  let log_collector = LogCollector::new()
  
  // 收集日志
  let collected_logs = LogCollector::collect(log_collector, logger_provider)
  
  // 验证日志收集
  assert_true(collected_logs.length() >= 4)
  
  // 验证日志级别
  let debug_logs = collected_logs.filter(|log| log.severity_number == Debug)
  let info_logs = collected_logs.filter(|log| log.severity_number == Info)
  let warn_logs = collected_logs.filter(|log| log.severity_number == Warn)
  let error_logs = collected_logs.filter(|log| log.severity_number == Error)
  
  assert_true(debug_logs.length() >= 1)
  assert_true(info_logs.length() >= 1)
  assert_true(warn_logs.length() >= 1)
  assert_true(error_logs.length() >= 1)
  
  // 验证日志与追踪的关联
  for log in collected_logs {
    assert_eq(log.trace_id, Some(SpanContext::trace_id(span_context)))
    assert_eq(log.span_id, Some(SpanContext::span_id(span_context)))
  }
  
  // 验证日志属性
  let error_log_data = error_logs[0]
  assert_eq(error_log_data.body, Some("Operation failed"))
  assert_eq(error_log_data.attributes.get("error.code"), Some(StringValue("DB_TIMEOUT")))
  assert_eq(error_log_data.attributes.get("error.retry_count"), Some(IntValue(3)))
}

// 测试4: 健康检查和状态监控测试
test "健康检查和状态监控测试" {
  // 创建健康检查管理器
  let health_manager = HealthCheckManager::new()
  
  // 添加各种健康检查
  let database_check = HealthCheck::new("database", "Database connectivity check")
  HealthCheck::set_check_function(database_check, || {
    // 模拟数据库健康检查
    true  // 假设数据库健康
  })
  
  let cache_check = HealthCheck::new("cache", "Cache service check")
  HealthCheck::set_check_function(cache_check, || {
    // 模拟缓存健康检查
    true  // 假设缓存健康
  })
  
  let external_service_check = HealthCheck::new("external_api", "External API connectivity check")
  HealthCheck::set_check_function(external_service_check, || {
    // 模拟外部API健康检查
    false  // 假设外部API不健康
  })
  
  // 注册健康检查
  HealthCheckManager::register(health_manager, database_check)
  HealthCheckManager::register(health_manager, cache_check)
  HealthCheckManager::register(health_manager, external_service_check)
  
  // 执行健康检查
  let health_results = HealthCheckManager::check_all(health_manager)
  
  // 验证健康检查结果
  assert_eq(health_results.length(), 3)
  
  let db_result = health_results.find(|result| result.name == "database")
  let cache_result = health_results.find(|result| result.name == "cache")
  let external_result = health_results.find(|result| result.name == "external_api")
  
  assert_true(db_result.is_some())
  assert_true(cache_result.is_some())
  assert_true(external_result.is_some())
  
  match (db_result, cache_result, external_result) {
    (Some(db), Some(cache), Some(external)) => {
      assert_true(db.healthy)
      assert_true(cache.healthy)
      assert_false(external.healthy)
    }
    _ => assert_true(false)
  }
  
  // 获取整体健康状态
  let overall_health = HealthCheckManager::get_overall_health(health_manager)
  assert_false(overall_health.healthy)  // 有一个检查失败，所以整体不健康
  
  // 创建状态监控器
  let status_monitor = StatusMonitor::new()
  
  // 添加状态指标
  StatusMonitor::add_metric(status_monitor, "cpu.usage", 65.5)
  StatusMonitor::add_metric(status_monitor, "memory.usage", 78.2)
  StatusMonitor::add_metric(status_monitor, "disk.usage", 45.0)
  StatusMonitor::add_metric(status_monitor, "network.latency", 25.3)
  
  // 添加状态标签
  StatusMonitor::add_label(status_monitor, "environment", "production")
  StatusMonitor::add_label(status_monitor, "region", "us-west-2")
  StatusMonitor::add_label(status_monitor, "instance", "web-01")
  
  // 获取状态报告
  let status_report = StatusMonitor::generate_report(status_monitor)
  
  // 验证状态报告
  assert_true(status_report.metrics.length() >= 4)
  assert_true(status_report.labels.length() >= 3)
  
  let cpu_metric = status_report.metrics.find(|metric| metric.name == "cpu.usage")
  assert_true(cpu_metric.is_some())
  
  match cpu_metric {
    Some(metric) => {
      assert_eq(metric.value, 65.5)
      assert_true(metric.timestamp > 0)
    }
    None => assert_true(false)
  }
  
  // 测试状态阈值检查
  StatusMonitor::set_threshold(status_monitor, "cpu.usage", 80.0)  // CPU使用率阈值80%
  StatusMonitor::set_threshold(status_monitor, "memory.usage", 90.0)  // 内存使用率阈值90%
  
  let threshold_violations = StatusMonitor::check_thresholds(status_monitor)
  
  // 验证阈值违规
  assert_true(threshold_violations.length() == 0)  // 没有违规，CPU 65.5% < 80%, 内存 78.2% < 90%
  
  // 更新指标以触发阈值违规
  StatusMonitor::add_metric(status_monitor, "cpu.usage", 85.0)
  StatusMonitor::add_metric(status_monitor, "memory.usage", 95.0)
  
  let new_threshold_violations = StatusMonitor::check_thresholds(status_monitor)
  
  // 验证新的阈值违规
  assert_true(new_threshold_violations.length() == 2)  // CPU和内存都超过阈值
}

// 测试5: 自定义仪表板和可视化测试
test "自定义仪表板和可视化测试" {
  // 创建仪表板管理器
  let dashboard_manager = DashboardManager::new()
  
  // 创建仪表板
  let dashboard = Dashboard::new("system.overview", "System Overview Dashboard")
  Dashboard::set_refresh_interval(dashboard, 30)  // 30秒刷新间隔
  
  // 添加面板到仪表板
  let request_rate_panel = Panel::new("request_rate", "Request Rate")
  Panel::set_type(request_rate_panel, "timeseries")
  Panel::set_metric(request_rate_panel, "http.requests.total")
  Panel::set_aggregation(request_rate_panel, "rate")
  Panel::set_time_range(request_rate_panel, "1h")  // 最近1小时
  
  let error_rate_panel = Panel::new("error_rate", "Error Rate")
  Panel::set_type(error_rate_panel, "timeseries")
  Panel::set_metric(error_rate_panel, "http.requests.total")
  Panel::set_filter(error_rate_panel, "status_code >= 400")
  Panel::set_aggregation(error_rate_panel, "rate")
  Panel::set_time_range(error_rate_panel, "1h")
  
  let response_time_panel = Panel::new("response_time", "Response Time")
  Panel::set_type(response_time_panel, "heatmap")
  Panel::set_metric(response_time_panel, "http.request.duration")
  Panel::set_time_range(response_time_panel, "1h")
  
  let system_resources_panel = Panel::new("system_resources", "System Resources")
  Panel::set_type(system_resources_panel, "stat")
  Panel::set_metrics(system_resources_panel, [
    "system.cpu.usage",
    "system.memory.usage",
    "system.disk.usage"
  ])
  
  // 添加面板到仪表板
  Dashboard::add_panel(dashboard, request_rate_panel)
  Dashboard::add_panel(dashboard, error_rate_panel)
  Dashboard::add_panel(dashboard, response_time_panel)
  Dashboard::add_panel(dashboard, system_resources_panel)
  
  // 注册仪表板
  DashboardManager::register(dashboard_manager, dashboard)
  
  // 生成仪表板数据
  let dashboard_data = DashboardManager::generate_data(dashboard_manager, "system.overview")
  
  // 验证仪表板数据
  assert_true(dashboard_data.panels.length() == 4)
  
  let request_rate_data = dashboard_data.panels.find(|panel| panel.name == "request_rate")
  assert_true(request_rate_data.is_some())
  
  match request_rate_data {
    Some(panel_data) => {
      assert_eq(panel_data.type, "timeseries")
      assert_true(panel_data.data_points.length() > 0)
    }
    None => assert_true(false)
  }
  
  // 测试仪表板模板
  let service_template = DashboardTemplate::new("service.overview", "Service Overview Template")
  DashboardTemplate::add_panel_template(service_template, {
    "name": "service_request_rate",
    "type": "timeseries",
    "metric": "http.requests.total",
    "filter": "service_name=\"{{service_name}}\"",
    "aggregation": "rate"
  })
  
  DashboardTemplate::add_panel_template(service_template, {
    "name": "service_error_rate",
    "type": "timeseries",
    "metric": "http.requests.total",
    "filter": "service_name=\"{{service_name}}\" AND status_code >= 400",
    "aggregation": "rate"
  })
  
  // 从模板创建仪表板
  let template_vars = {
    "service_name": "payment.service"
  }
  
  let service_dashboard = DashboardTemplate::instantiate(service_template, "payment.service", "Payment Service Dashboard", template_vars)
  
  // 验证模板实例化
  assert_eq(service_dashboard.name, "payment.service")
  assert_eq(service_dashboard.title, "Payment Service Dashboard")
  assert_eq(service_dashboard.panels.length(), 2)
  
  let payment_request_rate_panel = service_dashboard.panels.find(|panel| panel.name == "service_request_rate")
  assert_true(payment_request_rate_panel.is_some())
  
  match payment_request_rate_panel {
    Some(panel) => {
      assert_eq(panel.metric, Some("http.requests.total"))
      assert_eq(panel.filter, Some("service_name=\"payment.service\""))
    }
    None => assert_true(false)
  }
  
  // 测试仪表板导出
  let dashboard_json = Dashboard::to_json(dashboard)
  assert_true(dashboard_json.contains("system.overview"))
  assert_true(dashboard_json.contains("request_rate"))
  
  let imported_dashboard = Dashboard::from_json(dashboard_json)
  assert_eq(imported_dashboard.name, dashboard.name)
  assert_eq(imported_dashboard.title, dashboard.title)
  assert_eq(imported_dashboard.panels.length(), dashboard.panels.length())
}

// 测试6: 告警和通知测试
test "告警和通知测试" {
  // 创建告警管理器
  let alert_manager = AlertManager::new()
  
  // 创建告警规则
  let high_error_rate_rule = AlertRule::new("high_error_rate", "High Error Rate")
  AlertRule::set_condition(high_error_rate_rule, "rate(http.requests.total{status_code>=400}) > 0.1")
  AlertRule::set_duration(high_error_rate_rule, 300)  // 5分钟
  AlertRule::set_severity(high_error_rate_rule, "warning")
  AlertRule::set_message(high_error_rate_rule, "Error rate is {{ $value }} errors per second")
  
  let high_latency_rule = AlertRule::new("high_latency", "High Latency")
  AlertRule::set_condition(high_latency_rule, "histogram_quantile(0.95, http.request.duration) > 1.0")
  AlertRule::set_duration(high_latency_rule, 600)  // 10分钟
  AlertRule::set_severity(high_latency_rule, "critical")
  AlertRule::set_message(high_latency_rule, "95th percentile latency is {{ $value }} seconds")
  
  let system_down_rule = AlertRule::new("system_down", "System Down")
  AlertRule::set_condition(system_down_rule, "up{job=\"azimuth\"} == 0")
  AlertRule::set_duration(system_down_rule, 60)  // 1分钟
  AlertRule::set_severity(system_down_rule, "critical")
  AlertRule::set_message(system_down_rule, "Azimuth system is down")
  
  // 注册告警规则
  AlertManager::add_rule(alert_manager, high_error_rate_rule)
  AlertManager::add_rule(alert_manager, high_latency_rule)
  AlertManager::add_rule(alert_manager, system_down_rule)
  
  // 创建通知渠道
  let email_channel = NotificationChannel::new("email", "Email Notifications")
  NotificationChannel::set_type(email_channel, "email")
  NotificationChannel::set_config(email_channel, {
    "smtp_server": "smtp.example.com",
    "smtp_port": 587,
    "username": "alerts@example.com",
    "password": "secret",
    "from": "alerts@example.com",
    "to": ["ops@example.com", "dev@example.com"]
  })
  
  let slack_channel = NotificationChannel::new("slack", "Slack Notifications")
  NotificationChannel::set_type(slack_channel, "slack")
  NotificationChannel::set_config(slack_channel, {
    "webhook_url": "https://hooks.slack.com/services/...",
    "channel": "#alerts",
    "username": "AzimuthBot"
  })
  
  let pagerduty_channel = NotificationChannel::new("pagerduty", "PagerDuty Notifications")
  NotificationChannel::set_type(pagerduty_channel, "pagerduty")
  NotificationChannel::set_config(pagerduty_channel, {
    "service_key": "integration-key-12345",
    "severity": "critical"
  })
  
  // 注册通知渠道
  AlertManager::add_channel(alert_manager, email_channel)
  AlertManager::add_channel(alert_manager, slack_channel)
  AlertManager::add_channel(alert_manager, pagerduty_channel)
  
  // 创建告警路由
  let warning_route = AlertRoute::new("warning", "Warning Alerts")
  AlertRoute::add_severity_filter(warning_route, "warning")
  AlertRoute::add_channel(warning_route, "email")
  AlertRoute::add_channel(warning_route, "slack")
  
  let critical_route = AlertRoute::new("critical", "Critical Alerts")
  AlertRoute::add_severity_filter(critical_route, "critical")
  AlertRoute::add_channel(critical_route, "email")
  AlertRoute::add_channel(critical_route, "slack")
  AlertRoute::add_channel(critical_route, "pagerduty")
  
  // 注册告警路由
  AlertManager::add_route(alert_manager, warning_route)
  AlertManager::add_route(alert_manager, critical_route)
  
  // 模拟指标数据
  let metrics_data = [
    ("rate(http.requests.total{status_code>=400})", 0.15),  // 触发高错误率告警
    ("histogram_quantile(0.95, http.request.duration)", 1.2),  // 触发高延迟告警
    ("up{job=\"azimuth\"}", 1)  // 不触发系统down告警
  ]
  
  // 评估告警规则
  let alert_evaluations = AlertManager::evaluate_rules(alert_manager, metrics_data)
  
  // 验证告警评估结果
  assert_true(alert_evaluations.length() >= 2)  // 至少有两个告警触发
  
  let error_rate_alert = alert_evaluations.find(|alert| alert.rule_name == "high_error_rate")
  let latency_alert = alert_evaluations.find(|alert| alert.rule_name == "high_latency")
  
  assert_true(error_rate_alert.is_some())
  assert_true(latency_alert.is_some())
  
  match (error_rate_alert, latency_alert) {
    (Some(error_alert), Some(latency_alert)) => {
      assert_true(error_alert.firing)
      assert_eq(error_alert.severity, "warning")
      assert_true(error_alert.message.contains("0.15"))
      
      assert_true(latency_alert.firing)
      assert_eq(latency_alert.severity, "critical")
      assert_true(latency_alert.message.contains("1.2"))
    }
    _ => assert_true(false)
  }
  
  // 发送告警通知
  let notifications_sent = AlertManager::send_notifications(alert_manager, alert_evaluations)
  
  // 验证通知发送
  assert_true(notifications_sent >= 4)  // warning告警发送2个通知，critical告警发送3个通知
  
  // 测试告警抑制
  let suppression_rule = AlertSuppressionRule::new("maintenance_suppression", "Maintenance Suppression")
  AlertSuppressionRule::set_condition(suppression_rule, "labels.maintenance == \"true\"")
  AlertSuppressionRule::set_duration(suppression_rule, 3600)  // 1小时
  
  AlertManager::add_suppression_rule(alert_manager, suppression_rule)
  
  // 创建带有维护标签的告警
  let maintenance_alert = Alert::new("high_error_rate", "High Error Rate")
  Alert::set_label(maintenance_alert, "maintenance", "true")
  Alert::set_severity(maintenance_alert, "warning")
  
  // 检查告警是否被抑制
  let is_suppressed = AlertManager::is_suppressed(alert_manager, maintenance_alert)
  assert_true(is_suppressed)
}

// 测试7: 性能分析和剖析测试
test "性能分析和剖析测试" {
  // 创建性能分析器
  let profiler = Profiler::new()
  
  // 启动性能分析
  Profiler::start(profiler)
  
  // 模拟一些工作负载
  let work_items = []
  
  for i = 0; i < 1000; i = i + 1 {
    // 创建工作项
    let work_item = {
      "id": i,
      "type": match i % 3 {
        0 => "cpu_intensive"
        1 => "memory_intensive"
        _ => "io_intensive"
      },
      "start_time": Clock::now_unix_nanos(Clock::system())
    }
    
    // 模拟不同类型的工作
    match work_item.type {
      "cpu_intensive" => {
        // 模拟CPU密集型工作
        let result = 0
        for j = 0; j < 1000; j = j + 1 {
          result = result + j * j
        }
        work_item["result"] = result
      }
      "memory_intensive" => {
        // 模拟内存密集型工作
        let large_array = []
        for j = 0; j < 1000; j = j + 1 {
          large_array.push(j)
        }
        work_item["array_size"] = large_array.length()
      }
      "io_intensive" => {
        // 模拟IO密集型工作
        // 在真实环境中，这里会有实际的IO操作
        work_item["io_operations"] = 10
      }
      _ => {}
    }
    
    work_item["end_time"] = Clock::now_unix_nanos(Clock::system())
    work_items.push(work_item)
  }
  
  // 停止性能分析
  let profile_data = Profiler::stop(profiler)
  
  // 验证性能分析数据
  assert_true(profile_data.duration > 0)
  assert_true(profile_data.cpu_samples.length() > 0)
  assert_true(profile_data.memory_samples.length() > 0)
  
  // 分析CPU使用情况
  let cpu_analysis = Profiler::analyze_cpu_usage(profile_data)
  assert_true(cpu_analysis.total_time > 0)
  assert_true(cpu_analysis.hot_functions.length() > 0)
  
  // 分析内存使用情况
  let memory_analysis = Profiler::analyze_memory_usage(profile_data)
  assert_true(memory_analysis.peak_memory > 0)
  assert_true(memory_analysis.allocations.length() > 0)
  
  // 分析工作项性能
  let work_item_analysis = Profiler::analyze_work_items(work_items)
  
  let cpu_intensive_items = work_item_analysis.filter(|item| item.type == "cpu_intensive")
  let memory_intensive_items = work_item_analysis.filter(|item| item.type == "memory_intensive")
  let io_intensive_items = work_item_analysis.filter(|item| item.type == "io_intensive")
  
  assert_true(cpu_intensive_items.length() > 0)
  assert_true(memory_intensive_items.length() > 0)
  assert_true(io_intensive_items.length() > 0)
  
  // 计算平均执行时间
  let avg_cpu_time = cpu_intensive_items.reduce(0, |sum, item| sum + item.duration) / cpu_intensive_items.length()
  let avg_memory_time = memory_intensive_items.reduce(0, |sum, item| sum + item.duration) / memory_intensive_items.length()
  let avg_io_time = io_intensive_items.reduce(0, |sum, item| sum + item.duration) / io_intensive_items.length()
  
  assert_true(avg_cpu_time > 0)
  assert_true(avg_memory_time > 0)
  assert_true(avg_io_time > 0)
  
  // 生成性能报告
  let performance_report = Profiler::generate_report(profile_data, work_item_analysis)
  
  // 验证性能报告
  assert_true(performance_report.summary.length() > 0)
  assert_true(performance_report.cpu_analysis.length() > 0)
  assert_true(performance_report.memory_analysis.length() > 0)
  assert_true(performance_report.recommendations.length() > 0)
}

// 测试8: 自定义指标和仪表测试
test "自定义指标和仪表测试" {
  // 初始化自定义指标提供者
  let custom_meter_provider = MeterProvider::default()
  let custom_meter = MeterProvider::get_meter(custom_meter_provider, "custom.metrics.test")
  
  // 创建自定义指标
  let business_transactions = Meter::create_counter(custom_meter, "business.transactions.total")
  let business_revenue = Meter::create_counter(custom_meter, "business.revenue.total")
  let user_sessions = Meter::create_updown_counter(custom_meter, "business.user.sessions")
  let conversion_rate = Meter::create_gauge(custom_meter, "business.conversion.rate")
  let order_value = Meter::create_histogram(custom_meter, "business.order.value")
  
  // 模拟业务指标
  for i = 0; i < 1000; i = i + 1 {
    // 记录交易
    Counter::add(business_transactions, 1.0, [
      ("transaction.type", StringValue("purchase")),
      ("product.category", StringValue("electronics"))
    ])
    
    // 记录收入
    let order_amount = (i % 10 + 1) * 10.0
    Counter::add(business_revenue, order_amount, [
      ("transaction.type", StringValue("purchase"))
    ])
    
    // 记录订单价值
    Histogram::record(order_value, order_amount, [
      ("product.category", StringValue("electronics"))
    ])
  }
  
  // 模拟用户会话
  for i = 0; i < 500; i = i + 1 {
    UpDownCounter::add(user_sessions, 1.0, [
      ("user.type", StringValue("premium"))
    ])
  }
  
  // 计算并设置转化率
  let total_visitors = 10000.0
  let total_purchases = Counter::value(business_transactions)
  let calculated_conversion_rate = total_purchases / total_visitors
  Gauge::set(conversion_rate, calculated_conversion_rate)
  
  // 创建自定义仪表
  let business_dashboard = Dashboard::new("business.metrics", "Business Metrics Dashboard")
  
  // 添加业务面板
  let revenue_panel = Panel::new("revenue", "Total Revenue")
  Panel::set_type(revenue_panel, "stat")
  Panel::set_metric(revenue_panel, "business.revenue.total")
  Panel::set_unit(revenue_panel, "currency")
  
  let transactions_panel = Panel::new("transactions", "Total Transactions")
  Panel::set_type(transactions_panel, "stat")
  Panel::set_metric(transactions_panel, "business.transactions.total")
  
  let conversion_panel = Panel::new("conversion", "Conversion Rate")
  Panel::set_type(conversion_panel, "stat")
  Panel::set_metric(conversion_panel, "business.conversion.rate")
  Panel::set_unit(conversion_panel, "percentage")
  
  let sessions_panel = Panel::new("sessions", "Active Sessions")
  Panel::set_type(sessions_panel, "stat")
  Panel::set_metric(sessions_panel, "business.user.sessions")
  
  let order_distribution_panel = Panel::new("order_distribution", "Order Value Distribution")
  Panel::set_type(order_distribution_panel, "histogram")
  Panel::set_metric(order_distribution_panel, "business.order.value")
  
  // 添加面板到仪表板
  Dashboard::add_panel(business_dashboard, revenue_panel)
  Dashboard::add_panel(business_dashboard, transactions_panel)
  Dashboard::add_panel(business_dashboard, conversion_panel)
  Dashboard::add_panel(business_dashboard, sessions_panel)
  Dashboard::add_panel(business_dashboard, order_distribution_panel)
  
  // 验证自定义指标
  assert_eq(Counter::value(business_transactions), 1000.0)
  assert_true(Counter::value(business_revenue) > 0.0)
  assert_eq(UpDownCounter::value(user_sessions), 500.0)
  assert_true(Gauge::value(conversion_rate) > 0.0)
  
  // 验证仪表板
  assert_eq(business_dashboard.panels.length(), 5)
  
  // 创建指标收集器
  let custom_collector = MetricCollector::new()
  
  // 收集自定义指标
  let custom_metrics = MetricCollector::collect(custom_collector, custom_meter_provider)
  
  // 验证自定义指标收集
  assert_true(custom_metrics.length() >= 5)
  
  let revenue_metric = custom_metrics.find(|metric| metric.name == "business.revenue.total")
  let transactions_metric = custom_metrics.find(|metric| metric.name == "business.transactions.total")
  let sessions_metric = custom_metrics.find(|metric| metric.name == "business.user.sessions")
  let conversion_metric = custom_metrics.find(|metric| metric.name == "business.conversion.rate")
  let order_value_metric = custom_metrics.find(|metric| metric.name == "business.order.value")
  
  assert_true(revenue_metric.is_some())
  assert_true(transactions_metric.is_some())
  assert_true(sessions_metric.is_some())
  assert_true(conversion_metric.is_some())
  assert_true(order_value_metric.is_some())
  
  // 验证指标属性
  match transactions_metric {
    Some(metric) => {
      let transaction_data_point = metric.data_points.find(|dp| dp.attributes.get("transaction.type") == Some(StringValue("purchase")))
      assert_true(transaction_data_point.is_some())
      
      match transaction_data_point {
        Some(dp) => {
          assert_eq(dp.value, 1000.0)
          assert_eq(dp.attributes.get("product.category"), Some(StringValue("electronics")))
        }
        None => assert_true(false)
      }
    }
    None => assert_true(false)
  }
}

// 测试9: 多维度指标分析测试
test "多维度指标分析测试" {
  // 初始化指标提供者
  let meter_provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(meter_provider, "multidimensional.test")
  
  // 创建多维指标
  let request_counter = Meter::create_counter(meter, "http.requests.total")
  let response_latency = Meter::create_histogram(meter, "http.request.duration")
  
  // 模拟多维指标数据
  let methods = ["GET", "POST", "PUT", "DELETE"]
  let status_codes = ["200", "201", "400", "404", "500"]
  let services = ["user.service", "order.service", "payment.service"]
  let regions = ["us-east-1", "us-west-2", "eu-west-1"]
  
  for i = 0; i < 10000; i = i + 1 {
    let method = methods[i % methods.length()]
    let status_code = status_codes[i % status_codes.length()]
    let service = services[i % services.length()]
    let region = regions[i % regions.length()]
    
    // 记录请求计数
    Counter::add(request_counter, 1.0, [
      ("method", StringValue(method)),
      ("status_code", StringValue(status_code)),
      ("service", StringValue(service)),
      ("region", StringValue(region))
    ])
    
    // 记录响应延迟
    let latency = (i % 100 + 1).to_float() * 0.001
    Histogram::record(response_latency, latency, [
      ("method", StringValue(method)),
      ("status_code", StringValue(status_code)),
      ("service", StringValue(service)),
      ("region", StringValue(region))
    ])
  }
  
  // 创建指标分析器
  let analyzer = MetricAnalyzer::new()
  
  // 收集指标
  let collector = MetricCollector::new()
  let metrics = MetricCollector::collect(collector, meter_provider)
  
  // 按服务分析请求计数
  let requests_by_service = MetricAnalyzer::group_by(analyzer, metrics, "http.requests.total", "service")
  
  // 验证按服务分组
  assert_eq(requests_by_service.length(), services.length())
  
  for service in services {
    let service_data = requests_by_service.find(|group| group.key == service)
    assert_true(service_data.is_some())
    
    match service_data {
      Some(data) => {
        assert_true(data.value > 0)
      }
      None => assert_true(false)
    }
  }
  
  // 按方法和状态码分析请求计数
  let requests_by_method_status = MetricAnalyzer::group_by_multiple(analyzer, metrics, "http.requests.total", ["method", "status_code"])
  
  // 验证多维度分组
  assert_true(requests_by_method_status.length() > 0)
  
  // 按服务分析响应延迟
  let latency_by_service = MetricAnalyzer::analyze_histogram_by(analyzer, metrics, "http.request.duration", "service")
  
  // 验证延迟分析
  assert_eq(latency_by_service.length(), services.length())
  
  for service_data in latency_by_service {
    assert_true(service_data.count > 0)
    assert_true(service_data.sum > 0)
    assert_true(service_data.avg > 0)
    assert_true(service_data.p95 > 0)
    assert_true(service_data.p99 > 0)
  }
  
  // 计算错误率
  let error_rates = MetricAnalyzer::calculate_error_rate(analyzer, metrics, "http.requests.total", ["400", "404", "500"])
  
  // 验证错误率计算
  assert_true(error_rates.length() > 0)
  
  for error_rate in error_rates {
    assert_true(error_rate.rate >= 0.0 && error_rate.rate <= 1.0)
  }
  
  // 生成分析报告
  let analysis_report = MetricAnalyzer::generate_report(analyzer, metrics)
  
  // 验证分析报告
  assert_true(analysis_report.summary.length() > 0)
  assert_true(analysis_report.dimensions.length() > 0)
  assert_true(analysis_report.insights.length() > 0)
}

// 测试10: 可观测性数据关联和分析测试
test "可观测性数据关联和分析测试" {
  // 初始化所有提供者
  let meter_provider = MeterProvider::default()
  let tracer_provider = TracerProvider::default()
  let logger_provider = LoggerProvider::default()
  
  let meter = MeterProvider::get_meter(meter_provider, "correlation.test")
  let tracer = TracerProvider::get_tracer(tracer_provider, "correlation.test")
  let logger = LoggerProvider::get_logger(logger_provider, "correlation.test")
  
  // 创建关联的追踪、指标和日志
  let trace_id = "trace-12345"
  let span_id = "span-67890"
  
  // 创建span
  let span = Tracer::start_span(tracer, "user.registration")
  Span::set_attribute(span, "user.id", StringValue("user123"))
  Span::set_attribute(span, "registration.method", StringValue("email"))
  
  let span_context = Span::context(span)
  
  // 记录指标
  let registration_counter = Meter::create_counter(meter, "user.registrations.total")
  Counter::add(registration_counter, 1.0, [
    ("user.id", StringValue("user123")),
    ("registration.method", StringValue("email"))
  ])
  
  let registration_duration = Meter::create_histogram(meter, "user.registration.duration")
  Histogram::record(registration_duration, 2.5, [
    ("user.id", StringValue("user123")),
    ("registration.method", StringValue("email"))
  ])
  
  // 记录日志
  let start_log = LogRecord::new_with_context(
    Info,
    Some("User registration started"),
    None,
    Some(Clock::now_unix_nanos(Clock::system())),
    None,
    Some(SpanContext::trace_id(span_context)),
    Some(SpanContext::span_id(span_context)),
    Some(span_context)
  )
  LogRecord::set_attribute(start_log, "user.id", StringValue("user123"))
  LogRecord::set_attribute(start_log, "registration.method", StringValue("email"))
  Logger::emit(logger, start_log)
  
  let validation_log = LogRecord::new_with_context(
    Debug,
    Some("User data validation"),
    Some("Validating user input data"),
    Some(Clock::now_unix_nanos(Clock::system())),
    None,
    Some(SpanContext::trace_id(span_context)),
    Some(SpanContext::span_id(span_context)),
    Some(span_context)
  )
  LogRecord::set_attribute(validation_log, "user.id", StringValue("user123"))
  LogRecord::set_attribute(validation_log, "validation.step", StringValue("email_format"))
  Logger::emit(logger, validation_log)
  
  let completion_log = LogRecord::new_with_context(
    Info,
    Some("User registration completed"),
    None,
    Some(Clock::now_unix_nanos(Clock::system())),
    None,
    Some(SpanContext::trace_id(span_context)),
    Some(SpanContext::span_id(span_context)),
    Some(span_context)
  )
  LogRecord::set_attribute(completion_log, "user.id", StringValue("user123"))
  LogRecord::set_attribute(completion_log, "registration.method", StringValue("email"))
  LogRecord::set_attribute(completion_log, "registration.success", BoolValue(true))
  Logger::emit(logger, completion_log)
  
  // 结束span
  Span::end(span)
  
  // 创建数据关联器
  let correlator = ObservabilityCorrelator::new()
  
  // 收集所有数据
  let metric_collector = MetricCollector::new()
  let trace_collector = TraceCollector::new()
  let log_collector = LogCollector::new()
  
  let metrics = MetricCollector::collect(metric_collector, meter_provider)
  let traces = TraceCollector::collect(trace_collector, tracer_provider)
  let logs = LogCollector::collect(log_collector, logger_provider)
  
  // 按用户ID关联数据
  let user_correlation = ObservabilityCorrelator::correlate_by_user(correlator, metrics, traces, logs, "user123")
  
  // 验证用户关联
  assert_true(user_correlation.metrics.length() > 0)
  assert_true(user_correlation.traces.length() > 0)
  assert_true(user_correlation.logs.length() > 0)
  
  // 验证所有数据都有相同的用户ID
  for metric in user_correlation.metrics {
    assert_eq(metric.attributes.get("user.id"), Some(StringValue("user123")))
  }
  
  for trace in user_correlation.traces {
    assert_eq(trace.attributes.get("user.id"), Some(StringValue("user123")))
  }
  
  for log in user_correlation.logs {
    assert_eq(log.attributes.get("user.id"), Some(StringValue("user123")))
  }
  
  // 按追踪ID关联数据
  let trace_correlation = ObservabilityCorrelator::correlate_by_trace(correlator, metrics, traces, logs, SpanContext::trace_id(span_context))
  
  // 验证追踪关联
  assert_true(trace_correlation.traces.length() > 0)
  assert_true(trace_correlation.logs.length() > 0)
  
  // 验证所有日志都有相同的追踪ID
  for log in trace_correlation.logs {
    assert_eq(log.trace_id, Some(SpanContext::trace_id(span_context)))
  }
  
  // 生成关联分析报告
  let correlation_report = ObservabilityCorrelator::generate_analysis_report(correlator, user_correlation)
  
  // 验证关联分析报告
  assert_true(correlation_report.user_id == "user123")
  assert_true(correlation_report.timeline.length() > 0)
  assert_true(correlation_report.metrics_summary.length() > 0)
  assert_true(correlation_report.traces_summary.length() > 0)
  assert_true(correlation_report.logs_summary.length() > 0)
  
  // 验证时间线顺序
  for i = 1; i < correlation_report.timeline.length(); i = i + 1 {
    let current_event = correlation_report.timeline[i]
    let previous_event = correlation_report.timeline[i - 1]
    assert_true(current_event.timestamp >= previous_event.timestamp)
  }
}