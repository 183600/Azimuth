// Azimuth 自定义度量测试用例
// 测试自定义度量指标和监控系统

// 测试1: 自定义计数器度量
test "自定义计数器度量" {
  // 创建度量提供者
  let meter_provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(meter_provider, "custom.metrics.test")
  
  // 创建自定义计数器
  let request_counter = Meter::create_counter(
    meter, 
    "http.requests.total", 
    Some("Total HTTP requests"), 
    Some("requests")
  )
  
  let error_counter = Meter::create_counter(
    meter, 
    "http.errors.total", 
    Some("Total HTTP errors"), 
    Some("errors")
  )
  
  // 测试基本计数器操作
  assert_eq(request_counter.name, "http.requests.total")
  assert_eq(request_counter.description, Some("Total HTTP requests"))
  assert_eq(request_counter.unit, Some("requests"))
  
  // 增加计数器
  Counter::add(request_counter, 1.0)
  Counter::add(request_counter, 5.0)
  Counter::add(request_counter, 10.0)
  
  // 测试带属性的计数器增加
  Counter::add_with_attributes(request_counter, 3.0, [
    ("method", "GET"),
    ("status", "200"),
    ("endpoint", "/api/users")
  ])
  
  Counter::add_with_attributes(request_counter, 2.0, [
    ("method", "POST"),
    ("status", "201"),
    ("endpoint", "/api/orders")
  ])
  
  Counter::add_with_attributes(request_counter, 1.0, [
    ("method", "GET"),
    ("status", "404"),
    ("endpoint", "/api/unknown")
  ])
  
  // 增加错误计数器
  Counter::add_with_attributes(error_counter, 1.0, [
    ("error_type", "ValidationError"),
    ("endpoint", "/api/users")
  ])
  
  Counter::add_with_attributes(error_counter, 2.0, [
    ("error_type", "AuthenticationError"),
    ("endpoint", "/api/auth")
  ])
  
  // 获取度量数据
  let request_metrics = Counter::get_measurements(request_counter)
  let error_metrics = Counter::get_measurements(error_counter)
  
  // 验证请求度量
  assert_eq(request_metrics.length(), 4)  // 3个无属性 + 3个有属性（可能合并）
  
  // 验证总请求数
  let total_requests = request_metrics.reduce(fn(acc, measurement) {
    acc + measurement.value
  }, 0.0)
  
  assert_eq(total_requests, 22.0)  // 1 + 5 + 10 + 3 + 2 + 1
  
  // 验证错误度量
  assert_eq(error_metrics.length(), 2)
  
  let total_errors = error_metrics.reduce(fn(acc, measurement) {
    acc + measurement.value
  }, 0.0)
  
  assert_eq(total_errors, 3.0)  // 1 + 2
  
  // 测试计数器重置
  Counter::reset(request_counter)
  let reset_metrics = Counter::get_measurements(request_counter)
  
  // 验证重置后的度量
  assert_eq(reset_metrics.length(), 0)
  
  // 重新添加数据
  Counter::add_with_attributes(request_counter, 5.0, [
    ("method", "GET"),
    ("status", "200"),
    ("endpoint", "/api/data")
  ])
  
  let new_metrics = Counter::get_measurements(request_counter)
  assert_eq(new_metrics.length(), 1)
  assert_eq(new_metrics[0].value, 5.0)
}

// 测试2: 自定义仪表盘度量
test "自定义仪表盘度量" {
  // 创建度量提供者
  let meter_provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(meter_provider, "gauge.metrics.test")
  
  // 创建自定义仪表盘
  let memory_usage_gauge = Meter::create_gauge(
    meter, 
    "system.memory.usage", 
    Some("Memory usage in bytes"), 
    Some("bytes")
  )
  
  let cpu_usage_gauge = Meter::create_gauge(
    meter, 
    "system.cpu.usage", 
    Some("CPU usage percentage"), 
    Some("percent")
  )
  
  let active_connections_gauge = Meter::create_gauge(
    meter, 
    "network.connections.active", 
    Some("Active network connections"), 
    Some("connections")
  )
  
  // 测试仪表盘设置
  Gauge::set(memory_usage_gauge, 1073741824.0)  // 1GB
  Gauge::set_with_attributes(cpu_usage_gauge, 75.5, [
    ("core", "0"),
    ("mode", "user")
  ])
  
  Gauge::set_with_attributes(cpu_usage_gauge, 25.0, [
    ("core", "0"),
    ("mode", "system")
  ])
  
  Gauge::set_with_attributes(cpu_usage_gauge, 60.0, [
    ("core", "1"),
    ("mode", "user")
  ])
  
  Gauge::set_with_attributes(cpu_usage_gauge, 15.0, [
    ("core", "1"),
    ("mode", "system")
  ])
  
  Gauge::set(active_connections_gauge, 150.0)
  
  // 获取仪表盘度量
  let memory_metrics = Gauge::get_measurements(memory_usage_gauge)
  let cpu_metrics = Gauge::get_measurements(cpu_usage_gauge)
  let connection_metrics = Gauge::get_measurements(active_connections_gauge)
  
  // 验证内存度量
  assert_eq(memory_metrics.length(), 1)
  assert_eq(memory_metrics[0].value, 1073741824.0)
  
  // 验证CPU度量
  assert_eq(cpu_metrics.length(), 4)
  
  // 验证特定CPU度量
  let cpu_core0_user = cpu_metrics.filter(fn(m) {
    m.attributes.contains(("core", "0")) && m.attributes.contains(("mode", "user"))
  })
  
  assert_eq(cpu_core0_user.length(), 1)
  assert_eq(cpu_core0_user[0].value, 75.5)
  
  // 验证连接度量
  assert_eq(connection_metrics.length(), 1)
  assert_eq(connection_metrics[0].value, 150.0)
  
  // 测试仪表盘更新
  Gauge::set(memory_usage_gauge, 2147483648.0)  // 2GB
  Gauge::set(active_connections_gauge, 175.0)
  
  // 获取更新后的度量
  let updated_memory_metrics = Gauge::get_measurements(memory_usage_gauge)
  let updated_connection_metrics = Gauge::get_measurements(active_connections_gauge)
  
  // 验证更新后的度量
  assert_eq(updated_memory_metrics[0].value, 2147483648.0)
  assert_eq(updated_connection_metrics[0].value, 175.0)
  
  // 测试仪表盘递增和递减
  Gauge::increment(memory_usage_gauge, 1073741824.0)  // 增加1GB
  Gauge::decrement(active_connections_gauge, 25.0)    // 减少25个连接
  
  let final_memory_metrics = Gauge::get_measurements(memory_usage_gauge)
  let final_connection_metrics = Gauge::get_measurements(active_connections_gauge)
  
  // 验证递增和递减操作
  assert_eq(final_memory_metrics[0].value, 3221225472.0)  // 2GB + 1GB
  assert_eq(final_connection_metrics[0].value, 150.0)     // 175 - 25
}

// 测试3: 自定义直方图度量
test "自定义直方图度量" {
  // 创建度量提供者
  let meter_provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(meter_provider, "histogram.metrics.test")
  
  // 创建自定义直方图
  let response_time_histogram = Meter::create_histogram(
    meter, 
    "http.request.duration", 
    Some("HTTP request duration"), 
    Some("ms")
  )
  
  let payload_size_histogram = Meter::create_histogram(
    meter, 
    "http.request.size", 
    Some("HTTP request payload size"), 
    Some("bytes")
  )
  
  // 记录响应时间数据
  let response_times = [45.2, 123.7, 78.9, 256.1, 89.3, 167.8, 234.5, 12.5, 345.6, 67.8]
  
  for time in response_times {
    Histogram::record(response_time_histogram, time)
  }
  
  // 记录带属性的响应时间
  Histogram::record_with_attributes(response_time_histogram, 156.8, [
    ("endpoint", "/api/users"),
    ("method", "GET")
  ])
  
  Histogram::record_with_attributes(response_time_histogram, 234.5, [
    ("endpoint", "/api/orders"),
    ("method", "POST")
  ])
  
  Histogram::record_with_attributes(response_time_histogram, 89.7, [
    ("endpoint", "/api/products"),
    ("method", "GET")
  ])
  
  // 记录负载大小数据
  let payload_sizes = [1024.0, 2048.0, 4096.0, 8192.0, 16384.0, 512.0, 256.0, 128.0]
  
  for size in payload_sizes {
    Histogram::record(payload_size_histogram, size)
  }
  
  // 获取直方图度量
  let response_time_metrics = Histogram::get_measurements(response_time_histogram)
  let payload_size_metrics = Histogram::get_measurements(payload_size_histogram)
  
  // 验证响应时间度量
  assert_eq(response_time_metrics.length(), 4)  // 1个无属性 + 3个有属性
  
  // 验证响应时间统计
  let default_response_times = response_time_metrics.filter(fn(m) {
    m.attributes.length() == 0
  })
  
  assert_eq(default_response_times.length(), 1)
  
  let response_time_stats = default_response_times[0].histogram_stats
  assert_eq(response_time_stats.count, 10)
  assert_eq(response_time_stats.sum, 1421.4)
  assert_eq(response_time_stats.min, 12.5)
  assert_eq(response_time_stats.max, 345.6)
  
  // 验证特定端点的响应时间
  let users_endpoint_times = response_time_metrics.filter(fn(m) {
    m.attributes.contains(("endpoint", "/api/users"))
  })
  
  assert_eq(users_endpoint_times.length(), 1)
  assert_eq(users_endpoint_times[0].histogram_stats.count, 1)
  assert_eq(users_endpoint_times[0].histogram_stats.sum, 156.8)
  
  // 验证负载大小度量
  assert_eq(payload_size_metrics.length(), 1)
  
  let payload_size_stats = payload_size_metrics[0].histogram_stats
  assert_eq(payload_size_stats.count, 8)
  assert_eq(payload_size_stats.sum, 33640.0)
  assert_eq(payload_size_stats.min, 128.0)
  assert_eq(payload_size_stats.max, 16384.0)
  
  // 验证百分位数
  let p50 = calculate_percentile(response_times, 50.0)
  let p95 = calculate_percentile(response_times, 95.0)
  let p99 = calculate_percentile(response_times, 99.0)
  
  assert_true(p50 > 70.0 && p50 < 90.0)
  assert_true(p95 > 200.0 && p95 < 350.0)
  assert_true(p99 > 300.0 && p99 < 350.0)
}

// 测试4: 自定义度量聚合
test "自定义度量聚合" {
  // 创建度量提供者
  let meter_provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(meter_provider, "aggregation.metrics.test")
  
  // 创建多个计数器
  let api_counter = Meter::create_counter(meter, "api.requests", Some("API requests"), Some("requests"))
  let web_counter = Meter::create_counter(meter, "web.requests", Some("Web requests"), Some("requests"))
  let mobile_counter = Meter::create_counter(meter, "mobile.requests", Some("Mobile requests"), Some("requests"))
  
  // 添加不同来源的请求数据
  Counter::add_with_attributes(api_counter, 100.0, [("endpoint", "/users"), ("method", "GET")])
  Counter::add_with_attributes(api_counter, 50.0, [("endpoint", "/orders"), ("method", "POST")])
  Counter::add_with_attributes(api_counter, 75.0, [("endpoint", "/products"), ("method", "GET")])
  
  Counter::add_with_attributes(web_counter, 200.0, [("page", "/home"), ("method", "GET")])
  Counter::add_with_attributes(web_counter, 150.0, [("page", "/dashboard"), ("method", "GET")])
  Counter::add_with_attributes(web_counter, 80.0, [("page", "/profile"), ("method", "GET")])
  
  Counter::add_with_attributes(mobile_counter, 120.0, [("screen", "/home"), ("method", "GET")])
  Counter::add_with_attributes(mobile_counter, 90.0, [("screen", "/search"), ("method", "GET")])
  Counter::add_with_attributes(mobile_counter, 60.0, [("screen", "/profile"), ("method", "GET")])
  
  // 创建度量聚合器
  let aggregator = MetricAggregator()
  
  // 添加度量源
  aggregator.add_source("api", api_counter)
  aggregator.add_source("web", web_counter)
  aggregator.add_source("mobile", mobile_counter)
  
  // 测试总和聚合
  let total_requests = aggregator.sum("requests")
  assert_eq(total_requests, 925.0)  // 100+50+75+200+150+80+120+90+60
  
  // 测试按来源分组聚合
  let requests_by_source = aggregator.group_by_source("requests")
  
  assert_eq(requests_by_source.length(), 3)
  assert_eq(requests_by_source.get("api"), Some(225.0))  // 100+50+75
  assert_eq(requests_by_source.get("web"), Some(430.0))  // 200+150+80
  assert_eq(requests_by_source.get("mobile"), Some(270.0))  // 120+90+60
  
  // 测试按属性分组聚合
  let get_requests = aggregator.filter_by_attribute("method", "GET").sum("requests")
  let post_requests = aggregator.filter_by_attribute("method", "POST").sum("requests")
  
  assert_eq(get_requests, 875.0)  // 100+75+200+150+80+120+90+60
  assert_eq(post_requests, 50.0)  // 50
  
  // 测试时间窗口聚合
  let time_window_aggregator = TimeWindowAggregator({
    window_size_ms: 60000,  // 1分钟窗口
    max_windows: 10         // 保留10个窗口
  })
  
  // 添加时间序列数据
  let base_time = get_current_timestamp()
  
  for i in 0..5 {
    let timestamp = base_time + (i * 10000)  // 每10秒一个数据点
    time_window_aggregator.add_metric("requests", 100.0 + (i * 10.0), timestamp)
  }
  
  // 获取当前窗口的聚合
  let current_window = time_window_aggregator.get_current_window()
  assert_eq(current_window.sum, 150.0)  // 最后一个数据点
  assert_eq(current_window.count, 1)
  
  // 获取所有窗口的聚合
  let all_windows = time_window_aggregator.get_all_windows()
  assert_eq(all_windows.length(), 5)
  
  // 验证时间序列聚合
  let total_time_series = all_windows.reduce(fn(acc, window) {
    acc + window.sum
  }, 0.0)
  
  assert_eq(total_time_series, 650.0)  // 100+110+120+130+140+150
  
  // 测试比率计算
  let error_counter = Meter::create_counter(meter, "errors.total", Some("Total errors"), Some("errors"))
  
  Counter::add_with_attributes(error_counter, 10.0, [("source", "api")])
  Counter::add_with_attributes(error_counter, 5.0, [("source", "web")])
  Counter::add_with_attributes(error_counter, 8.0, [("source", "mobile")])
  
  aggregator.add_source("errors", error_counter)
  
  // 计算错误率
  let total_errors = aggregator.sum("errors")
  let error_rate = aggregator.calculate_ratio("errors", "requests")
  
  assert_eq(total_errors, 23.0)  // 10+5+8
  assert_eq(error_rate, 23.0 / 925.0)
  
  // 按来源计算错误率
  let api_error_rate = aggregator.calculate_ratio_for_source("errors", "requests", "api")
  let web_error_rate = aggregator.calculate_ratio_for_source("errors", "requests", "web")
  let mobile_error_rate = aggregator.calculate_ratio_for_source("errors", "requests", "mobile")
  
  assert_eq(api_error_rate, 10.0 / 225.0)
  assert_eq(web_error_rate, 5.0 / 430.0)
  assert_eq(mobile_error_rate, 8.0 / 270.0)
}

// 测试5: 自定义度量导出
test "自定义度量导出" {
  // 创建度量提供者
  let meter_provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(meter_provider, "export.metrics.test")
  
  // 创建各种类型的度量
  let counter = Meter::create_counter(meter, "test.counter", Some("Test counter"), Some("items"))
  let gauge = Meter::create_gauge(meter, "test.gauge", Some("Test gauge"), Some("units"))
  let histogram = Meter::create_histogram(meter, "test.histogram", Some("Test histogram"), Some("ms"))
  
  // 添加度量数据
  Counter::add_with_attributes(counter, 100.0, [("env", "test")])
  Gauge::set_with_attributes(gauge, 42.5, [("env", "test")])
  Histogram::record_with_attributes(histogram, 123.4, [("env", "test")])
  
  // 测试Prometheus格式导出
  let prometheus_exporter = PrometheusExporter()
  let prometheus_output = prometheus_exporter.export(meter)
  
  // 验证Prometheus格式
  assert_true(prometheus_output.contains("# HELP test_counter Test counter"))
  assert_true(prometheus_output.contains("# TYPE test_counter counter"))
  assert_true(prometheus_output.contains("test_counter{env=\"test\"} 100"))
  
  assert_true(prometheus_output.contains("# HELP test_gauge Test gauge"))
  assert_true(prometheus_output.contains("# TYPE test_gauge gauge"))
  assert_true(prometheus_output.contains("test_gauge{env=\"test\"} 42.5"))
  
  assert_true(prometheus_output.contains("# HELP test_histogram Test histogram"))
  assert_true(prometheus_output.contains("# TYPE test_histogram histogram"))
  assert_true(prometheus_output.contains("test_histogram_sum{env=\"test\"} 123.4"))
  assert_true(prometheus_output.contains("test_histogram_count{env=\"test\"} 1"))
  
  // 测试JSON格式导出
  let json_exporter = JsonExporter()
  let json_output = json_exporter.export(meter)
  
  // 验证JSON格式
  assert_true(json_output.contains("\"test_counter\""))
  assert_true(json_output.contains("\"test_gauge\""))
  assert_true(json_output.contains("\"test_histogram\""))
  assert_true(json_output.contains("\"env\":\"test\""))
  
  // 测试InfluxDB格式导出
  let influxdb_exporter = InfluxDBExporter()
  let influxdb_output = influxdb_exporter.export(meter)
  
  // 验证InfluxDB格式
  assert_true(influxdb_output.contains("test_counter"))
  assert_true(influxdb_output.contains("env=test"))
  assert_true(influxdb_output.contains("value=100"))
  
  // 测试自定义格式导出
  let custom_exporter = CustomExporter({
    format_template: "{metric_name}.{attribute_key}={attribute_value} {metric_value}",
    separator: "\n"
  })
  
  let custom_output = custom_exporter.export(meter)
  
  // 验证自定义格式
  assert_true(custom_output.contains("test_counter.env=test 100"))
  assert_true(custom_output.contains("test_gauge.env=test 42.5"))
  
  // 测试批量导出
  let batch_exporter = BatchExporter({
    exporters: [prometheus_exporter, json_exporter, influxdb_exporter],
    output_format: "combined"
  })
  
  let batch_output = batch_exporter.export(meter)
  
  // 验证批量导出包含所有格式
  assert_true(batch_output.contains("# HELP test_counter"))  // Prometheus
  assert_true(batch_output.contains("\"test_counter\""))      // JSON
  assert_true(batch_output.contains("test_counter"))         // InfluxDB
  
  // 测试带时间戳的导出
  let timestamped_exporter = TimestampedExporter({
    base_exporter: prometheus_exporter,
    timestamp_format: "unix_ms"
  })
  
  let timestamped_output = timestamped_exporter.export(meter)
  
  // 验证时间戳包含在输出中
  assert_true(timestamped_output.contains("# HELP test_counter"))
  assert_true(timestamped_output.contains("test_counter{env=\"test\"} 100"))
  assert_true(timestamped_output.contains("timestamp="))
}

// 测试6: 自定义度量仪表盘
test "自定义度量仪表盘" {
  // 创建仪表盘管理器
  let dashboard_manager = DashboardManager()
  
  // 创建仪表盘
  let system_dashboard = Dashboard({
    id: "system_overview",
    name: "System Overview",
    description: "System performance metrics",
    refresh_interval_ms: 5000,
    panels: []
  })
  
  // 添加面板到仪表盘
  let cpu_panel = Panel({
    id: "cpu_usage",
    title: "CPU Usage",
    type: "gauge_chart",
    position: { x: 0, y: 0, width: 6, height: 4 },
    query: "system.cpu.usage",
    options: {
      "max": 100,
      "unit": "percent",
      "thresholds": [70, 90]
    }
  })
  
  let memory_panel = Panel({
    id: "memory_usage",
    title: "Memory Usage",
    type: "line_chart",
    position: { x: 6, y: 0, width: 6, height: 4 },
    query: "system.memory.usage",
    options: {
      "time_range": "1h",
      "unit": "bytes"
    }
  })
  
  let request_rate_panel = Panel({
    id: "request_rate",
    title: "Request Rate",
    type: "bar_chart",
    position: { x: 0, y: 4, width: 12, height: 4 },
    query: "http.requests.rate",
    options: {
      "time_range": "24h",
      "group_by": "endpoint"
    }
  })
  
  // 添加面板到仪表盘
  dashboard_manager.add_panel(system_dashboard, cpu_panel)
  dashboard_manager.add_panel(system_dashboard, memory_panel)
  dashboard_manager.add_panel(system_dashboard, request_rate_panel)
  
  // 验证仪表盘创建
  let created_dashboard = dashboard_manager.get_dashboard("system_overview")
  match created_dashboard {
    Some(dashboard) => {
      assert_eq(dashboard.name, "System Overview")
      assert_eq(dashboard.panels.length(), 3)
    }
    None => assert_true(false)
  }
  
  // 添加仪表盘到管理器
  dashboard_manager.add_dashboard(system_dashboard)
  
  // 测试仪表盘渲染
  let renderer = DashboardRenderer()
  let rendered_dashboard = renderer.render(system_dashboard)
  
  // 验证渲染结果
  assert_true(rendered_dashboard.contains("System Overview"))
  assert_true(rendered_dashboard.contains("CPU Usage"))
  assert_true(rendered_dashboard.contains("Memory Usage"))
  assert_true(rendered_dashboard.contains("Request Rate"))
  
  // 测试面板数据查询
  let query_engine = MetricQueryEngine()
  
  // 模拟度量数据
  let mock_metrics = [
    ("system.cpu.usage", 75.5, [("core", "0")]),
    ("system.cpu.usage", 60.0, [("core", "1")]),
    ("system.memory.usage", 4294967296.0, []),
    ("http.requests.rate", 150.0, [("endpoint", "/api/users")]),
    ("http.requests.rate", 75.0, [("endpoint", "/api/orders")]),
    ("http.requests.rate", 200.0, [("endpoint", "/api/products")])
  ]
  
  for metric in mock_metrics {
    query_engine.add_metric_data(metric.0, metric.1, metric.2)
  }
  
  // 查询CPU使用率
  let cpu_data = query_engine.query("system.cpu.usage")
  assert_eq(cpu_data.length(), 2)
  
  // 查询内存使用率
  let memory_data = query_engine.query("system.memory.usage")
  assert_eq(memory_data.length(), 1)
  assert_eq(memory_data[0].value, 4294967296.0)
  
  // 查询请求率
  let request_data = query_engine.query("http.requests.rate")
  assert_eq(request_data.length(), 3)
  
  // 测试聚合查询
  let avg_cpu = query_engine.aggregate("system.cpu.usage", "avg")
  let max_memory = query_engine.aggregate("system.memory.usage", "max")
  let sum_requests = query_engine.aggregate("http.requests.rate", "sum")
  
  assert_eq(avg_cpu, 67.75)  // (75.5 + 60.0) / 2
  assert_eq(max_memory, 4294967296.0)
  assert_eq(sum_requests, 425.0)  // 150 + 75 + 200
  
  // 测试过滤查询
  let core0_cpu = query_engine.query_with_filter("system.cpu.usage", [("core", "0")])
  let users_requests = query_engine.query_with_filter("http.requests.rate", [("endpoint", "/api/users")])
  
  assert_eq(core0_cpu.length(), 1)
  assert_eq(core0_cpu[0].value, 75.5)
  assert_eq(core0_cpu[0].attributes.get("core"), Some("0"))
  
  assert_eq(users_requests.length(), 1)
  assert_eq(users_requests[0].value, 150.0)
  assert_eq(users_requests[0].attributes.get("endpoint"), Some("/api/users"))
  
  // 测试时间序列查询
  let time_series_data = []
  let base_time = get_current_timestamp()
  
  for i in 0..10 {
    let timestamp = base_time + (i * 60000)  // 每分钟一个数据点
    let value = 100.0 + (i * 10.0) + (Random::next() * 20.0 - 10.0)  // 带随机波动的增长趋势
    time_series_data.push((timestamp, value))
    query_engine.add_time_series_metric("system.cpu.usage", value, timestamp, [("core", "0")])
  }
  
  // 查询时间序列数据
  let cpu_time_series = query_engine.query_time_range(
    "system.cpu.usage", 
    base_time, 
    base_time + 600000,  // 10分钟
    [("core", "0")]
  )
  
  assert_eq(cpu_time_series.length(), 10)
  
  // 验证时间序列数据
  for i in 0..cpu_time_series.length() {
    let data_point = cpu_time_series[i]
    assert_eq(data_point.timestamp, base_time + (i * 60000))
    assert_true(data_point.value >= 90.0)  // 100 + (i * 10) - 10
    assert_true(data_point.value <= 130.0) // 100 + (i * 10) + 10
  }
  
  // 测试仪表盘模板
  let dashboard_template = DashboardTemplate({
    name: "Service Template",
    description: "Template for service monitoring",
    panels: [
      PanelTemplate({
        title: "Request Rate",
        type: "line_chart",
        query: "{service}.requests.rate",
        variables: ["service"]
      }),
      PanelTemplate({
        title: "Error Rate",
        type: "single_stat",
        query: "{service}.errors.rate",
        variables: ["service"]
      }),
      PanelTemplate({
        title: "Response Time",
        type: "histogram",
        query: "{service}.response.time",
        variables: ["service"]
      })
    ]
  })
  
  // 从模板创建仪表盘
  let service_dashboard = dashboard_manager.create_from_template(
    dashboard_template, 
    [("service", "api_service")]
  )
  
  // 验证模板创建的仪表盘
  assert_eq(service_dashboard.panels.length(), 3)
  
  let request_rate_panel = service_dashboard.panels[0]
  assert_eq(request_rate_panel.title, "Request Rate")
  assert_eq(request_rate_panel.query, "api_service.requests.rate")
  
  let error_rate_panel = service_dashboard.panels[1]
  assert_eq(error_rate_panel.title, "Error Rate")
  assert_eq(error_rate_panel.query, "api_service.errors.rate")
  
  let response_time_panel = service_dashboard.panels[2]
  assert_eq(response_time_panel.title, "Response Time")
  assert_eq(response_time_panel.query, "api_service.response.time")
}