// Azimuth 自定义遥测仪表板测试用例
// 专注于自定义遥测仪表板的创建、配置和功能测试

// 测试1: 仪表板基础组件创建
test "仪表板基础组件创建测试" {
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "dashboard.components")
  
  // 创建仪表板管理器
  let dashboard_manager = DashboardManager::new(meter)
  
  // 创建组件指标
  let component_counter = Meter::create_counter(meter, "dashboard.components.created")
  let widget_gauge = Meter::create_gauge(meter, "dashboard.widgets.active")
  
  // 创建基础仪表板
  let dashboard = DashboardManager::create_dashboard(dashboard_manager, "system.overview", [
    ("title", StringValue("系统概览")),
    ("description", StringValue("系统整体运行状态监控")),
    ("refresh.interval", IntValue(30)),
    ("layout", StringValue("grid"))
  ])
  Counter::add(component_counter, 1.0)
  
  // 创建各种仪表板组件
  let metric_chart = DashboardManager::create_metric_chart(dashboard_manager, "cpu.usage.chart", [
    ("type", StringValue("line")),
    ("metric", StringValue("system.cpu.usage")),
    ("title", StringValue("CPU使用率")),
    ("time.range", StringValue("1h"))
  ])
  Counter::add(component_counter, 1.0)
  
  let gauge_widget = DashboardManager::create_gauge_widget(dashboard_manager, "memory.gauge", [
    ("metric", StringValue("system.memory.usage")),
    ("title", StringValue("内存使用率")),
    ("min", FloatValue(0.0)),
    ("max", FloatValue(100.0)),
    ("thresholds", StringValue("70,90"))
  ])
  Counter::add(component_counter, 1.0)
  
  let alert_panel = DashboardManager::create_alert_panel(dashboard_manager, "system.alerts", [
    ("title", StringValue("系统告警")),
    ("severity", StringValue("warning,critical")),
    ("max.items", IntValue(10))
  ])
  Counter::add(component_counter, 1.0)
  
  // 添加组件到仪表板
  DashboardManager::add_component(dashboard_manager, dashboard, metric_chart)
  DashboardManager::add_component(dashboard_manager, dashboard, gauge_widget)
  DashboardManager::add_component(dashboard_manager, dashboard, alert_panel)
  
  Gauge::set(widget_gauge, 3.0)
  
  // 验证组件创建
  let dashboard_info = DashboardManager::get_dashboard_info(dashboard_manager, dashboard)
  assert_eq(dashboard_info.component_count, 3)
  assert_eq(dashboard_info.title, "系统概览")
  assert_eq(Counter::value(component_counter), 4.0)
  assert_eq(Gauge::value(widget_gauge), 3.0)
}

// 测试2: 仪表板数据源配置
test "仪表板数据源配置测试" {
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "dashboard.datasource")
  
  // 创建数据源管理器
  let datasource_manager = DatasourceManager::new(meter)
  
  // 创建数据源指标
  let datasource_counter = Meter::create_counter(meter, "dashboard.datasources.configured")
  let query_gauge = Meter::create_gauge(meter, "dashboard.queries.active")
  
  // 配置多种数据源
  let prometheus_ds = DatasourceManager::configure_prometheus(datasource_manager, "prometheus.prod", [
    ("url", StringValue("http://prometheus.prod:9090")),
    ("timeout", IntValue(30)),
    ("scrape.interval", IntValue(15))
  ])
  Counter::add(datasource_counter, 1.0)
  
  let influxdb_ds = DatasourceManager::configure_influxdb(datasource_manager, "influxdb.timeseries", [
    ("url", StringValue("http://influxdb:8086")),
    ("database", StringValue("telemetry")),
    ("username", StringValue("teleuser")),
    ("retention.policy", StringValue("autogen"))
  ])
  Counter::add(datasource_counter, 1.0)
  
  let elasticsearch_ds = DatasourceManager::configure_elasticsearch(datasource_manager, "elasticsearch.logs", [
    ("url", StringValue("http://elasticsearch:9200")),
    ("index.pattern", StringValue("logs-*")),
    ("time.field", StringValue("@timestamp"))
  ])
  Counter::add(datasource_counter, 1.0)
  
  // 创建数据查询
  let cpu_query = DatasourceManager::create_query(datasource_manager, prometheus_ds, [
    ("query", StringValue("rate(cpu_usage_total[5m])")),
    ("legend", StringValue("{{instance}}")),
    ("format", StringValue("time_series"))
  ])
  
  let memory_query = DatasourceManager::create_query(datasource_manager, influxdb_ds, [
    ("query", StringValue("SELECT mean(usage) FROM memory_usage WHERE time > now() - 1h GROUP BY time(5m)")),
    ("format", StringValue("time_series"))
  ])
  
  let logs_query = DatasourceManager::create_query(datasource_manager, elasticsearch_ds, [
    ("query", StringValue("level:ERROR AND timestamp:[now-1h TO now]")),
    ("format", StringValue("table"))
  ])
  
  Gauge::set(query_gauge, 3.0)
  
  // 验证数据源配置
  let datasource_info = DatasourceManager::get_info(datasource_manager)
  assert_eq(datasource_info.total_datasources, 3)
  assert_eq(datasource_info.total_queries, 3)
  assert_eq(Counter::value(datasource_counter), 3.0)
  assert_eq(Gauge::value(query_gauge), 3.0)
}

// 测试3: 仪表板布局和响应式设计
test "仪表板布局和响应式设计测试" {
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "dashboard.layout")
  
  // 创建布局管理器
  let layout_manager = LayoutManager::new(meter)
  
  // 创建布局指标
  let layout_counter = Meter::create_counter(meter, "dashboard.layouts.created")
  let responsive_gauge = Meter::create_gauge(meter, "dashboard.breakpoints.active")
  
  // 创建响应式仪表板布局
  let responsive_layout = LayoutManager::create_responsive_layout(layout_manager, "main.dashboard", [
    ("desktop.columns", IntValue(12)),
    ("tablet.columns", IntValue(8)),
    ("mobile.columns", IntValue(4)),
    ("gutter.size", IntValue(16))
  ])
  Counter::add(layout_counter, 1.0)
  
  // 定义网格布局
  let grid_layout = LayoutManager::define_grid_layout(layout_manager, responsive_layout, [
    ("widget.cpu", GridLayoutItem::new(0, 0, 6, 4)), // 宽6高4
    ("widget.memory", GridLayoutItem::new(6, 0, 6, 4)), // 宽6高4
    ("widget.network", GridLayoutItem::new(0, 4, 8, 3)), // 宽8高3
    ("widget.alerts", GridLayoutItem::new(8, 4, 4, 3)), // 宽4高3
    ("widget.logs", GridLayoutItem::new(0, 7, 12, 5)) // 宽12高5
  ])
  
  // 定义响应式断点
  let breakpoints = [
    Breakpoint::new("mobile", 0, 768),
    Breakpoint::new("tablet", 768, 1024),
    Breakpoint::new("desktop", 1024, 999999)
  ]
  
  for breakpoint in breakpoints {
    LayoutManager::add_breakpoint(layout_manager, responsive_layout, breakpoint)
  }
  
  Gauge::set(responsive_gauge, breakpoints.length().to_float())
  
  // 验证布局配置
  let layout_info = LayoutManager::get_layout_info(layout_manager, responsive_layout)
  assert_eq(layout_info.total_widgets, 5)
  assert_eq(layout_info.breakpoints, 3)
  assert_eq(Counter::value(layout_counter), 1.0)
  assert_eq(Gauge::value(responsive_gauge), 3.0)
}

// 测试4: 仪表板主题和样式定制
test "仪表板主题和样式定制测试" {
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "dashboard.theme")
  
  // 创建主题管理器
  let theme_manager = ThemeManager::new(meter)
  
  // 创建主题指标
  let theme_counter = Meter::create_counter(meter, "dashboard.themes.created")
  let style_gauge = Meter::create_gauge(meter, "dashboard.styles.active")
  
  // 创建自定义主题
  let dark_theme = ThemeManager::create_theme(theme_manager, "dark.mode", [
    ("primary.color", StringValue("#1890ff")),
    ("background.color", StringValue("#141414")),
    ("text.color", StringValue("#ffffff")),
    ("border.color", StringValue("#434343")),
    ("success.color", StringValue("#52c41a")),
    ("warning.color", StringValue("#faad14")),
    ("error.color", StringValue("#f5222d"))
  ])
  Counter::add(theme_counter, 1.0)
  
  let light_theme = ThemeManager::create_theme(theme_manager, "light.mode", [
    ("primary.color", StringValue("#1890ff")),
    ("background.color", StringValue("#ffffff")),
    ("text.color", StringValue("#000000")),
    ("border.color", StringValue("#d9d9d9")),
    ("success.color", StringValue("#52c41a")),
    ("warning.color", StringValue("#faad14")),
    ("error.color", StringValue("#f5222d"))
  ])
  Counter::add(theme_counter, 1.0)
  
  // 创建自定义样式
  let custom_styles = [
    CustomStyle::new("chart.title", [
      ("font.size", StringValue("16px")),
      ("font.weight", StringValue("bold")),
      ("color", StringValue("primary.color"))
    ]),
    CustomStyle::new("panel.border", [
      ("border.width", StringValue("1px")),
      ("border.style", StringValue("solid")),
      ("border.color", StringValue("border.color")),
      ("border.radius", StringValue("4px"))
    ]),
    CustomStyle::new("metric.value", [
      ("font.size", StringValue("24px")),
      ("font.weight", StringValue("600")),
      ("color", StringValue("text.color"))
    ])
  ]
  
  for style in custom_styles {
    ThemeManager::add_custom_style(theme_manager, dark_theme, style)
    ThemeManager::add_custom_style(theme_manager, light_theme, style)
  }
  
  Gauge::set(style_gauge, (custom_styles.length() * 2).to_float())
  
  // 验证主题配置
  let theme_info = ThemeManager::get_theme_info(theme_manager)
  assert_eq(theme_info.total_themes, 2)
  assert_eq(theme_info.custom_styles, 6)
  assert_eq(Counter::value(theme_counter), 2.0)
  assert_eq(Gauge::value(style_gauge), 6.0)
}

// 测试5: 仪表板实时数据更新
test "仪表板实时数据更新测试" {
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "dashboard.realtime")
  
  // 创建实时数据管理器
  let realtime_manager = RealtimeDataManager::new(meter)
  
  // 创建实时数据指标
  let update_counter = Meter::create_counter(meter, "dashboard.realtime.updates")
  let latency_histogram = Meter::create_histogram(meter, "dashboard.realtime.latency")
  let connection_gauge = Meter::create_gauge(meter, "dashboard.realtime.connections")
  
  // 配置实时数据源
  let websocket_source = RealtimeDataManager::configure_websocket(realtime_manager, "realtime.telemetry", [
    ("url", StringValue("ws://telemetry.stream:8080")),
    ("reconnect.interval", IntValue(5)),
    ("max.retries", IntValue(10))
  ])
  
  // 订阅实时数据流
  let subscriptions = [
    RealtimeDataManager::subscribe(realtime_manager, websocket_source, "cpu.usage", [
      ("update.interval", IntValue(1)),
      ("buffer.size", IntValue(100)
    ]),
    RealtimeDataManager::subscribe(realtime_manager, websocket_source, "memory.usage", [
      ("update.interval", IntValue(2)),
      ("buffer.size", IntValue(50))
    ]),
    RealtimeDataManager::subscribe(realtime_manager, websocket_source, "network.throughput", [
      ("update.interval", IntValue(1)),
      ("buffer.size", IntValue(200))
    ])
  ]
  
  Gauge::set(connection_gauge, 1.0)
  
  // 模拟实时数据更新
  let update_count = 50
  for i in 0..=update_count {
    let update_start = Time::now()
    
    // 模拟接收实时数据
    let realtime_data = [
      ("cpu.usage", FloatValue(30.0 + (Random::float() * 40.0))),
      ("memory.usage", FloatValue(50.0 + (Random::float() * 30.0))),
      ("network.throughput", FloatValue(100.0 + (Random::float() * 500.0)))
    ]
    
    for (metric, value) in realtime_data {
      RealtimeDataManager::process_update(realtime_manager, metric, value)
    }
    
    let update_end = Time::now()
    let update_latency = Time::duration_between(update_start, update_end)
    Histogram::record(latency_histogram, update_latency)
    
    Counter::add(update_counter, 1.0)
  }
  
  // 验证实时数据更新
  let realtime_info = RealtimeDataManager::get_info(realtime_manager)
  assert_eq(realtime_info.total_updates, update_count + 1)
  assert_eq(realtime_info.active_subscriptions, 3)
  assert_eq(Counter::value(update_counter), (update_count + 1).to_float())
  assert_eq(Gauge::value(connection_gauge), 1.0)
}

// 测试6: 仪表板用户交互和过滤
test "仪表板用户交互和过滤测试" {
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "dashboard.interaction")
  
  // 创建交互管理器
  let interaction_manager = InteractionManager::new(meter)
  
  // 创建交互指标
  let interaction_counter = Meter::create_counter(meter, "dashboard.interactions.total")
  let filter_gauge = Meter::create_gauge(meter, "dashboard.filters.active")
  
  // 配置交互过滤器
  let time_filter = InteractionManager::create_time_filter(interaction_manager, "time.range", [
    ("default.range", StringValue("1h")),
    ("options", StringValue("15m,30m,1h,6h,12h,24h")),
    ("allow.custom", BoolValue(true))
  ])
  
  let service_filter = InteractionManager::create_label_filter(interaction_manager, "service.filter", [
    ("label", StringValue("service")),
    ("options", StringValue("web-api,database,cache,queue")),
    ("multi.select", BoolValue(true))
  ])
  
  let severity_filter = InteractionManager::create_value_filter(interaction_manager, "severity.filter", [
    ("field", StringValue("severity")),
    ("type", StringValue("select")),
    ("options", StringValue("info,warning,error,critical"))
  ])
  
  // 模拟用户交互
  let interactions = [
    ("time.range.changed", "1h", "6h"),
    ("service.selected", "web-api", ""),
    ("service.selected", "database", ""),
    ("severity.changed", "warning", "error"),
    ("drill.down", "cpu.usage", "server-1"),
    ("zoom.in", "chart", "time.range"),
    ("export.data", "csv", "time.range")
  ]
  
  for (action_type, target, value) in interactions {
    InteractionManager::record_interaction(interaction_manager, action_type, target, value)
    Counter::add(interaction_counter, 1.0)
  }
  
  // 应用过滤器
  InteractionManager::apply_filter(interaction_manager, time_filter, "6h")
  InteractionManager::apply_filter(interaction_manager, service_filter, "web-api,database")
  InteractionManager::apply_filter(interaction_manager, severity_filter, "error")
  
  Gauge::set(filter_gauge, 3.0)
  
  // 验证交互和过滤
  let interaction_info = InteractionManager::get_info(interaction_manager)
  assert_eq(interaction_info.total_interactions, 7)
  assert_eq(interaction_info.active_filters, 3)
  assert_eq(Counter::value(interaction_counter), 7.0)
  assert_eq(Gauge::value(filter_gauge), 3.0)
}

// 测试7: 仪表板警报和通知
test "仪表板警报和通知测试" {
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "dashboard.alerts")
  
  // 创建警报管理器
  let alert_manager = AlertManager::new(meter)
  
  // 创建警报指标
  let alert_counter = Meter::create_counter(meter, "dashboard.alerts.triggered")
  let notification_gauge = Meter::create_gauge(meter, "dashboard.notifications.sent")
  
  // 配置警报规则
  let cpu_alert_rule = AlertManager::create_threshold_rule(alert_manager, "cpu.high.usage", [
    ("metric", StringValue("system.cpu.usage")),
    ("operator", StringValue(">")),
    ("threshold", FloatValue(80.0)),
    ("duration", IntValue(300)), // 5分钟
    ("severity", StringValue("warning"))
  ])
  
  let memory_alert_rule = AlertManager::create_threshold_rule(alert_manager, "memory.high.usage", [
    ("metric", StringValue("system.memory.usage")),
    ("operator", StringValue(">")),
    ("threshold", FloatValue(90.0)),
    ("duration", IntValue(180)), // 3分钟
    ("severity", StringValue("critical"))
  ])
  
  let error_rate_rule = AlertManager::create_rate_rule(alert_manager, "error.rate.high", [
    ("metric", StringValue("http.requests.error.rate")),
    ("operator", StringValue(">")),
    ("threshold", FloatValue(0.05)), // 5%
    ("window", StringValue("5m")),
    ("severity", StringValue("warning"))
  ])
  
  // 配置通知渠道
  let email_channel = AlertManager::configure_email_channel(alert_manager, "email.notifications", [
    ("smtp.server", StringValue("smtp.company.com")),
    ("from", StringValue("alerts@company.com")),
    ("to", StringValue("ops-team@company.com")),
    ("template", StringValue("alert.email.template"))
  ])
  
  let slack_channel = AlertManager::configure_slack_channel(alert_manager, "slack.notifications", [
    ("webhook.url", StringValue("https://hooks.slack.com/services/...")),
    ("channel", StringValue("#alerts")),
    ("username", StringValue("AzimuthBot"))
  ])
  
  // 模拟警报触发
  let alert_scenarios = [
    ("cpu.high.usage", 85.0, "warning"),
    ("memory.high.usage", 95.0, "critical"),
    ("error.rate.high", 0.08, "warning"),
    ("cpu.high.usage", 82.0, "warning")
  ]
  
  for (rule_name, value, severity) in alert_scenarios {
    let alert = AlertManager::evaluate_rule(alert_manager, rule_name, value)
    match alert {
      Some(a) => {
        AlertManager::send_notification(alert_manager, a, email_channel)
        AlertManager::send_notification(alert_manager, a, slack_channel)
        Counter::add(alert_counter, 1.0)
      }
      None => ()
    }
  }
  
  let notifications_sent = AlertManager::get_notification_count(alert_manager)
  Gauge::set(notification_gauge, notifications_sent.to_float())
  
  // 验证警报和通知
  let alert_info = AlertManager::get_info(alert_manager)
  assert_eq(alert_info.triggered_alerts, 4)
  assert_eq(alert_info.sent_notifications, notifications_sent)
  assert_eq(Counter::value(alert_counter), 4.0)
  assert_eq(Gauge::value(notification_gauge), notifications_sent.to_float())
}

// 测试8: 仪表板导出和分享
test "仪表板导出和分享测试" {
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "dashboard.export")
  
  // 创建导出管理器
  let export_manager = ExportManager::new(meter)
  
  // 创建导出指标
  let export_counter = Meter::create_counter(meter, "dashboard.exports.created")
  let share_gauge = Meter::create_gauge(mauge, "dashboard.shares.active")
  
  // 配置导出选项
  let export_options = [
    ExportOption::new("pdf", [
      ("format", StringValue("A4")),
      ("orientation", StringValue("landscape")),
      ("include.data", BoolValue(true))
    ]),
    ExportOption::new("png", [
      ("resolution", IntValue(1920)),
      ("quality", IntValue(90)),
      ("background", StringValue("white"))
    ]),
    ExportOption::new("json", [
      ("include.config", BoolValue(true)),
      ("include.data", BoolValue(false)),
      ("pretty.print", BoolValue(true))
    ]),
    ExportOption::new("csv", [
      ("delimiter", StringValue(",")),
      ("include.headers", BoolValue(true)),
      ("date.format", StringValue("ISO8601"))
    ])
  ]
  
  // 模拟仪表板导出
  for option in export_options {
    let export_result = ExportManager::export_dashboard(export_manager, "system.overview", option)
    match export_result {
      Success(file) => {
        Counter::add(export_counter, 1.0)
        assert_true(file.size > 0)
      }
      Error(_) => assert_true(false)
    }
  }
  
  // 配置分享选项
  let share_options = [
    ShareOption::new("public.link", [
      ("expires", StringValue("7d")),
      ("password.protected", BoolValue(false)),
      ("allow.download", BoolValue(true))
    ]),
    ShareOption::new("private.link", [
      ("expires", StringValue("24h")),
      ("password.protected", BoolValue(true)),
      ("allow.download", BoolValue(false))
    ]),
    ShareOption::new("embed", [
      ("width", IntValue(1200)),
      ("height", IntValue(800)),
      ("theme", StringValue("light"))
    ])
  ]
  
  // 模拟仪表板分享
  let mut share_links = []
  for option in share_options {
    let share_result = ExportManager::share_dashboard(export_manager, "system.overview", option)
    match share_result {
      Success(link) => {
        share_links = share_links.push(link)
      }
      Error(_) => assert_true(false)
    }
  }
  
  Gauge::set(share_gauge, share_links.length().to_float())
  
  // 验证导出和分享
  let export_info = ExportManager::get_info(export_manager)
  assert_eq(export_info.total_exports, 4)
  assert_eq(export_info.active_shares, 3)
  assert_eq(Counter::value(export_counter), 4.0)
  assert_eq(Gauge::value(share_gauge), 3.0)
}

// 测试9: 仪表板性能优化
test "仪表板性能优化测试" {
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "dashboard.performance")
  
  // 创建性能优化器
  let performance_optimizer = PerformanceOptimizer::new(meter)
  
  // 创建性能指标
  let optimization_counter = Meter::create_counter(meter, "dashboard.optimizations.applied")
  let cache_gauge = Meter::create_gauge(meter, "dashboard.cache.hit.rate")
  let load_time_histogram = Meter::create_histogram(meter, "dashboard.load.time")
  
  // 配置性能优化选项
  let caching_config = CachingConfig::new([
    ("query.cache ttl", IntValue(60)), // 1分钟
    ("dashboard.cache ttl", IntValue(300)), // 5分钟
    ("max.cache.size", IntValue(100)),
    ("cache.strategy", StringValue("lru"))
  ])
  
  let lazy_loading_config = LazyLoadingConfig::new([
    ("enable.lazy.loading", BoolValue(true)),
    ("load.threshold", IntValue(1000)), // 1秒
    ("placeholder.type", StringValue("skeleton"))
  ])
  
  let data_sampling_config = DataSamplingConfig::new([
    ("enable.sampling", BoolValue(true)),
    ("sample.rate", FloatValue(0.1)), // 10%
    ("min.data.points", IntValue(50))
  ])
  
  // 应用性能优化
  PerformanceOptimizer::apply_caching(performance_optimizer, caching_config)
  Counter::add(optimization_counter, 1.0)
  
  PerformanceOptimizer::apply_lazy_loading(performance_optimizer, lazy_loading_config)
  Counter::add(optimization_counter, 1.0)
  
  PerformanceOptimizer::apply_data_sampling(performance_optimizer, data_sampling_config)
  Counter::add(optimization_counter, 1.0)
  
  // 模拟仪表板加载性能测试
  let load_iterations = 10
  for i in 0..=load_iterations {
    let load_start = Time::now()
    
    // 模拟仪表板加载
    PerformanceOptimizer::simulate_dashboard_load(performance_optimizer, "system.overview")
    
    let load_end = Time::now()
    let load_time = Time::duration_between(load_start, load_end)
    Histogram::record(load_time_histogram, load_time)
  }
  
  // 计算缓存命中率
  let cache_hit_rate = PerformanceOptimizer::get_cache_hit_rate(performance_optimizer)
  Gauge::set(cache_gauge, cache_hit_rate)
  
  // 验证性能优化
  let performance_info = PerformanceOptimizer::get_info(performance_optimizer)
  assert_eq(performance_info.optimizations_applied, 3)
  assert_true(cache_hit_rate > 0.0)
  assert_eq(Counter::value(optimization_counter), 3.0)
  assert_eq(Gauge::value(cache_gauge), cache_hit_rate)
}

// 测试10: 仪表板权限和安全
test "仪表板权限和安全测试" {
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "dashboard.security")
  
  // 创建安全管理器
  let security_manager = SecurityManager::new(meter)
  
  // 创建安全指标
  let auth_counter = Meter::create_counter(meter, "dashboard.auth.attempts")
  let access_gauge = Meter::create_gauge(meter, "dashboard.access.granted")
  let block_counter = Meter::create_counter(meter, "dashboard.access.blocked")
  
  // 配置用户角色和权限
  let roles = [
    Role::new("admin", [
      Permission::new("dashboard.create", true),
      Permission::new("dashboard.edit", true),
      Permission::new("dashboard.delete", true),
      Permission::new("dashboard.share", true),
      Permission::new("dashboard.export", true)
    ]),
    Role::new("operator", [
      Permission::new("dashboard.create", false),
      Permission::new("dashboard.edit", true),
      Permission::new("dashboard.delete", false),
      Permission::new("dashboard.share", true),
      Permission::new("dashboard.export", true)
    ]),
    Role::new("viewer", [
      Permission::new("dashboard.create", false),
      Permission::new("dashboard.edit", false),
      Permission::new("dashboard.delete", false),
      Permission::new("dashboard.share", false),
      Permission::new("dashboard.export", true)
    ])
  ]
  
  // 创建用户
  let users = [
    User::new("admin.user", "admin", [
      ("department", StringValue("IT")),
      ("clearance.level", StringValue("high"))
    ]),
    User::new("operator.user", "operator", [
      ("department", StringValue("Operations")),
      ("clearance.level", StringValue("medium"))
    ]),
    User::new("viewer.user", "viewer", [
      ("department", StringValue("Business")),
      ("clearance.level", StringValue("low"))
    ])
  ]
  
  // 模拟用户访问控制
  let access_attempts = [
    ("admin.user", "dashboard.create", true),
    ("admin.user", "dashboard.delete", true),
    ("operator.user", "dashboard.create", false),
    ("operator.user", "dashboard.edit", true),
    ("viewer.user", "dashboard.edit", false),
    ("viewer.user", "dashboard.export", true),
    ("unauthorized.user", "dashboard.view", false)
  ]
  
  for (username, action, expected_result) in access_attempts {
    Counter::add(auth_counter, 1.0)
    
    let access_granted = SecurityManager::check_permission(security_manager, username, action)
    
    if access_granted {
      Gauge::set(access_gauge, Gauge::value(access_gauge) + 1.0)
    } else {
      Counter::add(block_counter, 1.0)
    }
    
    assert_eq(access_granted, expected_result)
  }
  
  // 验证权限和安全
  let security_info = SecurityManager::get_info(security_manager)
  assert_eq(security_info.total_auth_attempts, 7)
  assert_eq(security_info.access_granted, 4)
  assert_eq(security_info.access_blocked, 3)
  assert_eq(Counter::value(auth_counter), 7.0)
  assert_eq(Counter::value(block_counter), 3.0)
}