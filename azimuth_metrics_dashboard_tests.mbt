// Azimuth 度量仪表盘测试用例
// 测试度量仪表盘功能，包括数据可视化、实时更新和自定义配置

// 测试1: 仪表盘基本配置
test "仪表盘基本配置功能" {
  // 创建仪表盘配置
  let dashboard_config = DashboardConfig::new("系统性能仪表盘", "system.performance")
  
  // 添加面板配置
  DashboardConfig::add_panel(dashboard_config, PanelConfig::new(
    "CPU使用率", 
    "cpu.usage", 
    PanelType::Gauge,
    [("service", "api")]
  ))
  
  DashboardConfig::add_panel(dashboard_config, PanelConfig::new(
    "请求计数", 
    "http.requests.total", 
    PanelType::Counter,
    [("endpoint", "/api/data")]
  ))
  
  DashboardConfig::add_panel(dashboard_config, PanelConfig::new(
    "响应时间分布", 
    "http.response.time", 
    PanelType::Histogram,
    [("method", "GET")]
  ))
  
  // 验证配置
  let panel_count = DashboardConfig::panel_count(dashboard_config)
  let cpu_panel = DashboardConfig::get_panel(dashboard_config, "CPU使用率")
  let request_panel = DashboardConfig::get_panel(dashboard_config, "请求计数")
  
  assert_eq(panel_count, 3)
  assert_eq(PanelConfig::metric_name(cpu_panel), "cpu.usage")
  assert_eq(PanelConfig::panel_type(cpu_panel), PanelType::Gauge)
  assert_eq(PanelConfig::metric_name(request_panel), "http.requests.total")
  assert_eq(PanelConfig::panel_type(request_panel), PanelType::Counter)
}

// 测试2: 实时数据更新
test "实时数据更新功能" {
  // 创建仪表盘实例
  let dashboard = Dashboard::new("实时监控仪表盘")
  
  // 添加数据源
  let metrics_source = MetricsSource::new("application.metrics")
  Dashboard::add_data_source(dashboard, metrics_source)
  
  // 配置实时更新
  Dashboard::enable_realtime_updates(dashboard, 1000) // 1秒更新间隔
  
  // 添加度量数据点
  MetricsSource::add_gauge(metrics_source, "cpu.usage", 65.5, [("service", "web"), "timestamp", "1000"])
  MetricsSource::add_counter(metrics_source, "requests.total", 120.0, [("endpoint", "/api/users"), "timestamp", "1000"])
  MetricsSource::add_histogram(metrics_source, "response.time", 150.0, [("method", "GET"), "timestamp", "1000"])
  
  // 获取仪表盘数据
  let dashboard_data = Dashboard::get_current_data(dashboard)
  let cpu_data = Dashboard::get_metric_data(dashboard, "cpu.usage")
  let request_data = Dashboard::get_metric_data(dashboard, "requests.total")
  
  assert_eq(cpu_data.value, 65.5)
  assert_eq(request_data.value, 120.0)
  assert_eq(cpu_data.attributes[("service", "web")], ("service", "web"))
  
  // 更新数据
  MetricsSource::add_gauge(metrics_source, "cpu.usage", 72.3, [("service", "web"), "timestamp", "2000"])
  MetricsSource::add_counter(metrics_source, "requests.total", 135.0, [("endpoint", "/api/users"), "timestamp", "2000"])
  
  // 获取更新后的数据
  let updated_cpu_data = Dashboard::get_metric_data(dashboard, "cpu.usage")
  let updated_request_data = Dashboard::get_metric_data(dashboard, "requests.total")
  
  assert_eq(updated_cpu_data.value, 72.3)
  assert_eq(updated_request_data.value, 135.0)
  
  // 测试历史数据
  let cpu_history = Dashboard::get_metric_history(dashboard, "cpu.usage", 2)
  assert_eq(cpu_history.length(), 2)
  assert_eq(cpu_history[0].value, 65.5)
  assert_eq(cpu_history[1].value, 72.3)
}

// 测试3: 数据聚合和计算
test "数据聚合和计算功能" {
  // 创建仪表盘
  let dashboard = Dashboard::new("聚合数据仪表盘")
  
  // 添加数据源
  let metrics_source = MetricsSource::new("aggregation.test")
  Dashboard::add_data_source(dashboard, metrics_source)
  
  // 添加多个数据点
  for i = 0; i < 10; i = i + 1 {
    let timestamp = 1000 + i * 100
    let value = 50.0 + i.to_float() * 2.5
    MetricsSource::add_gauge(metrics_source, "cpu.usage", value, [("service", "api"), "timestamp", timestamp.to_string()])
  }
  
  // 创建聚合配置
  let aggregation_config = AggregationConfig::new()
  AggregationConfig::add_aggregator(aggregation_config, "cpu.usage.avg", AggregatorType::Average, "cpu.usage", 300) // 300ms窗口
  AggregationConfig::add_aggregator(aggregation_config, "cpu.usage.max", AggregatorType::Maximum, "cpu.usage", 300)
  AggregationConfig::add_aggregator(aggregation_config, "cpu.usage.min", AggregatorType::Minimum, "cpu.usage", 300)
  
  // 应用聚合配置
  Dashboard::apply_aggregation_config(dashboard, aggregation_config)
  
  // 获取聚合结果
  let avg_cpu = Dashboard::get_aggregated_metric(dashboard, "cpu.usage.avg")
  let max_cpu = Dashboard::get_aggregated_metric(dashboard, "cpu.usage.max")
  let min_cpu = Dashboard::get_aggregated_metric(dashboard, "cpu.usage.min")
  
  assert_true(avg_cpu > 60.0 && avg_cpu < 70.0)
  assert_eq(max_cpu, 72.5) // 最后一个数据点
  assert_eq(min_cpu, 50.0) // 第一个数据点
  
  // 测试百分位数聚合
  AggregationConfig::add_aggregator(aggregation_config, "cpu.usage.p95", AggregatorType::Percentile95, "cpu.usage", 1000)
  Dashboard::apply_aggregation_config(dashboard, aggregation_config)
  
  let p95_cpu = Dashboard::get_aggregated_metric(dashboard, "cpu.usage.p95")
  assert_true(p95_cpu > 70.0 && p95_cpu < 75.0)
}

// 测试4: 告警和阈值监控
test "告警和阈值监控功能" {
  // 创建仪表盘
  let dashboard = Dashboard::new("告警监控仪表盘")
  
  // 添加数据源
  let metrics_source = MetricsSource::new("alerting.test")
  Dashboard::add_data_source(dashboard, metrics_source)
  
  // 配置告警规则
  let alert_config = AlertConfig::new()
  
  // CPU使用率告警
  AlertConfig::add_threshold_rule(alert_config, "cpu.high", "cpu.usage", 
    ComparisonOperator::GreaterThan, 80.0, [("service", "api")])
  
  // 请求错误率告警
  AlertConfig::add_threshold_rule(alert_config, "error.rate.high", "error.rate", 
    ComparisonOperator::GreaterThan, 5.0, [])
  
  // 响应时间告警
  AlertConfig::add_threshold_rule(alert_config, "response.time.high", "response.time.p95", 
    ComparisonOperator::GreaterThan, 1000.0, [("endpoint", "/api/data")])
  
  // 应用告警配置
  Dashboard::apply_alert_config(dashboard, alert_config)
  
  // 添加正常数据
  MetricsSource::add_gauge(metrics_source, "cpu.usage", 45.0, [("service", "api")])
  MetricsSource::add_gauge(metrics_source, "error.rate", 2.5, [])
  MetricsSource::add_gauge(metrics_source, "response.time.p95", 800.0, [("endpoint", "/api/data")])
  
  // 检查告警状态
  let alerts = Dashboard::get_active_alerts(dashboard)
  assert_eq(alerts.length(), 0) // 应该没有告警
  
  // 添加触发告警的数据
  MetricsSource::add_gauge(metrics_source, "cpu.usage", 85.0, [("service", "api")])
  MetricsSource::add_gauge(metrics_source, "error.rate", 6.2, [])
  
  // 检查告警状态
  let new_alerts = Dashboard::get_active_alerts(dashboard)
  assert_eq(new_alerts.length(), 2) // 应该有两个告警
  
  // 验证告警内容
  let cpu_alert = new_alerts.filter(fn(a) { Alert::rule_id(a) == "cpu.high" })[0]
  let error_alert = new_alerts.filter(fn(a) { Alert::rule_id(a) == "error.rate.high" })[0]
  
  assert_eq(Alert::metric_name(cpu_alert), "cpu.usage")
  assert_eq(Alert::current_value(cpu_alert), 85.0)
  assert_eq(Alert::threshold(cpu_alert), 80.0)
  
  assert_eq(Alert::metric_name(error_alert), "error.rate")
  assert_eq(Alert::current_value(error_alert), 6.2)
  assert_eq(Alert::threshold(error_alert), 5.0)
  
  // 测试告警恢复
  MetricsSource::add_gauge(metrics_source, "cpu.usage", 65.0, [("service", "api")])
  
  let recovered_alerts = Dashboard::get_active_alerts(dashboard)
  let recovered_cpu_alerts = recovered_alerts.filter(fn(a) { Alert::rule_id(a) == "cpu.high" })
  assert_eq(recovered_cpu_alerts.length(), 0) // CPU告警应该已恢复
  
  // 测试告警历史
  let alert_history = Dashboard::get_alert_history(dashboard, 10)
  assert_true(alert_history.length() >= 2) // 至少有两个历史告警记录
}

// 测试5: 自定义面板配置
test "自定义面板配置功能" {
  // 创建仪表盘
  let dashboard = Dashboard::new("自定义面板仪表盘")
  
  // 创建自定义面板配置
  let custom_panel_config = CustomPanelConfig::new("自定义性能面板")
  
  // 添加图表配置
  CustomPanelConfig::add_chart(custom_panel_config, ChartConfig::new(
    "CPU趋势图",
    ChartType::Line,
    ["cpu.usage"],
    [("service", "api")],
    TimeRange::LastHour,
    [("title", "CPU使用率趋势"), ("yAxis", "百分比(%)")]
  ))
  
  CustomPanelConfig::add_chart(custom_panel_config, ChartConfig::new(
    "请求分布",
    ChartType::Pie,
    ["http.requests.get", "http.requests.post", "http.requests.put"],
    [],
    TimeRange::LastHour,
    [("title", "HTTP请求方法分布")]
  ))
  
  CustomPanelConfig::add_chart(custom_panel_config, ChartConfig::new(
    "响应时间热力图",
    ChartType::Heatmap,
    ["response.time"],
    [("endpoint", "/api/*")],
    TimeRange::LastDay,
    [("title", "API响应时间热力图"), ("colorScale", "YlOrRd")]
  ))
  
  // 应用自定义配置
  Dashboard::apply_custom_panel_config(dashboard, custom_panel_config)
  
  // 验证配置
  let charts = Dashboard::get_custom_charts(dashboard)
  assert_eq(charts.length(), 3)
  
  let cpu_chart = charts.filter(fn(c) { ChartConfig::title(c) == "CPU趋势图" })[0]
  let pie_chart = charts.filter(fn(c) { ChartConfig::title(c) == "请求分布" })[0]
  let heatmap_chart = charts.filter(fn(c) { ChartConfig::title(c) == "响应时间热力图" })[0]
  
  assert_eq(ChartConfig::chart_type(cpu_chart), ChartType::Line)
  assert_eq(ChartConfig::chart_type(pie_chart), ChartType::Pie)
  assert_eq(ChartConfig::chart_type(heatmap_chart), ChartType::Heatmap)
  
  // 测试图表数据获取
  let metrics_source = MetricsSource::new("custom.panel.test")
  Dashboard::add_data_source(dashboard, metrics_source)
  
  // 添加测试数据
  MetricsSource::add_gauge(metrics_source, "cpu.usage", 65.5, [("service", "api"), "timestamp", "1000"])
  MetricsSource::add_counter(metrics_source, "http.requests.get", 120.0, [("timestamp", "1000")])
  MetricsSource::add_counter(metrics_source, "http.requests.post", 45.0, [("timestamp", "1000")])
  MetricsSource::add_counter(metrics_source, "http.requests.put", 15.0, [("timestamp", "1000")])
  
  // 获取图表数据
  let cpu_chart_data = Dashboard::get_chart_data(dashboard, "CPU趋势图")
  let pie_chart_data = Dashboard::get_chart_data(dashboard, "请求分布")
  
  assert_true(cpu_chart_data.series.length() >= 1)
  assert_true(pie_chart_data.series.length() >= 3)
}

// 测试6: 仪表盘模板和导出
test "仪表盘模板和导出功能" {
  // 创建仪表盘模板
  let dashboard_template = DashboardTemplate::new("系统监控模板")
  
  // 添加模板面板
  DashboardTemplate::add_panel_template(dashboard_template, PanelTemplate::new(
    "系统CPU",
    "cpu.usage",
    PanelType::Gauge,
    [("service", "{service}")],
    [("title", "{service} CPU使用率"), ("unit", "%")]
  ))
  
  DashboardTemplate::add_panel_template(dashboard_template, PanelTemplate::new(
    "系统内存",
    "memory.usage",
    PanelType::Gauge,
    [("service", "{service}")],
    [("title", "{service} 内存使用率"), ("unit", "%")]
  ))
  
  DashboardTemplate::add_panel_template(dashboard_template, PanelTemplate::new(
    "请求计数",
    "http.requests.total",
    PanelType::Counter,
    [("service", "{service}")],
    [("title", "{service} 请求总数"), ("unit", "count")]
  ))
  
  // 从模板创建仪表盘
  let dashboard1 = DashboardTemplate::create_dashboard(dashboard_template, "API服务监控", 
    [("service", "api")])
  let dashboard2 = DashboardTemplate::create_dashboard(dashboard_template, "Web服务监控", 
    [("service", "web")])
  
  // 验证仪表盘创建
  assert_eq(Dashboard::panel_count(dashboard1), 3)
  assert_eq(Dashboard::panel_count(dashboard2), 3)
  
  let api_cpu_panel = Dashboard::get_panel(dashboard1, "API服务 CPU使用率")
  let web_cpu_panel = Dashboard::get_panel(dashboard2, "Web服务 CPU使用率")
  
  assert_eq(PanelConfig::metric_name(api_cpu_panel), "cpu.usage")
  assert_eq(PanelConfig::metric_name(web_cpu_panel), "cpu.usage")
  
  // 测试仪表盘导出
  let dashboard_json = Dashboard::export_to_json(dashboard1)
  assert_true(dashboard_json.length() > 0)
  assert_true(dashboard_json.contains("API服务监控"))
  
  // 测试仪表盘导入
  let imported_dashboard = Dashboard::import_from_json(dashboard_json)
  assert_eq(Dashboard::name(imported_dashboard), "API服务监控")
  assert_eq(Dashboard::panel_count(imported_dashboard), 3)
  
  // 测试仪表盘配置保存
  let config_saved = Dashboard::save_config(dashboard1, "/tmp/api_dashboard.config")
  assert_true(config_saved)
  
  let loaded_dashboard = Dashboard::load_config("/tmp/api_dashboard.config")
  assert_eq(Dashboard::name(loaded_dashboard), "API服务监控")
  assert_eq(Dashboard::panel_count(loaded_dashboard), 3)
}