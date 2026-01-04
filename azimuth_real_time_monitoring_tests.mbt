// Azimuth 实时监控测试用例
// 测试实时监控和告警系统功能

// 测试1: 实时数据流处理
test "实时数据流处理" {
  // 创建流处理器
  let stream_processor = StreamProcessor({
    buffer_size: 1000,
    batch_size: 100,
    processing_interval_ms: 1000
  })
  
  // 创建数据源
  let data_source = DataSource({
    name: "system_metrics",
    type: "push",
    endpoint: "ws://monitoring.example.com/metrics"
  })
  
  // 注册流处理器
  stream_processor.register_source(data_source)
  
  // 创建实时数据处理函数
  let process_metric = fn(data_point) {
    // 增加时间戳
    let enriched_data = {
      value = data_point.value,
      timestamp = get_current_timestamp(),
      source = data_point.source,
      metric_name = data_point.metric_name,
      attributes = data_point.attributes
    }
    
    // 应用转换
    if enriched_data.metric_name == "cpu_usage" {
      enriched_data.value = enriched_data.value * 100.0  // 转换为百分比
    }
    
    enriched_data
  }
  
  // 注册处理函数
  stream_processor.register_processor("metrics_transform", process_metric)
  
  // 模拟实时数据流
  let test_data_points = []
  let base_time = get_current_timestamp()
  
  for i in 0..500 {
    let data_point = DataPoint({
      value: 0.5 + (Random::next() * 0.5),  // 0.5-1.0之间的随机值
      metric_name: if i % 3 == 0 { "cpu_usage" } else if i % 3 == 1 { "memory_usage" } else { "disk_usage" },
      source: "server_" + (i % 5).to_string(),
      attributes: [
        ("region", "us-west-" + (i % 3).to_string()),
        ("environment", if i % 2 == 0 { "prod" } else { "staging" })
      ]
    })
    test_data_points.push(data_point)
  }
  
  // 处理数据流
  let processed_data = []
  for data_point in test_data_points {
    let processed = stream_processor.process(data_point)
    processed_data.push(processed)
  }
  
  // 验证数据处理
  assert_eq(processed_data.length(), 500)
  
  // 验证CPU使用率转换
  let cpu_points = processed_data.filter(fn(p) { p.metric_name == "cpu_usage" })
  for point in cpu_points {
    assert_true(point.value >= 50.0)  // 0.5 * 100
    assert_true(point.value <= 100.0) // 1.0 * 100
    assert_true(point.timestamp > 0)
  }
  
  // 验证时间戳添加
  for point in processed_data {
    assert_true(point.timestamp >= base_time)
  }
  
  // 测试窗口聚合
  let window_aggregator = WindowAggregator({
    window_size_ms: 5000,  // 5秒窗口
    aggregation_functions: ["avg", "max", "min", "count"]
  })
  
  // 添加时间窗口数据
  for i in 0..100 {
    let timestamp = base_time + (i * 100)  // 每100ms一个数据点
    let value = 50.0 + (Random::next() * 50.0)
    window_aggregator.add_data("cpu_usage", value, timestamp)
  }
  
  // 获取窗口聚合结果
  let window_results = window_aggregator.get_windows(base_time, base_time + 10000)
  
  // 验证窗口聚合
  assert_true(window_results.length() > 0)
  
  for window in window_results {
    assert_true(window.aggregations.contains("avg"))
    assert_true(window.aggregations.contains("max"))
    assert_true(window.aggregations.contains("min"))
    assert_true(window.aggregations.contains("count"))
    
    let avg = window.aggregations.get("avg")
    let max = window.aggregations.get("max")
    let min = window.aggregations.get("min")
    let count = window.aggregations.get("count")
    
    match avg {
      Some(v) => assert_true(v >= 50.0 && v <= 100.0)
      None => assert_true(false)
    }
    
    match max {
      Some(v) => assert_true(v >= 50.0 && v <= 100.0)
      None => assert_true(false)
    }
    
    match min {
      Some(v) => assert_true(v >= 50.0 && v <= 100.0)
      None => assert_true(false)
    }
    
    match count {
      Some(v) => assert_true(v > 0)
      None => assert_true(false)
    }
  }
}

// 测试2: 实时告警系统
test "实时告警系统" {
  // 创建告警管理器
  let alert_manager = AlertManager({
    evaluation_interval_ms: 5000,
    for_duration_ms: 15000,  // 告警持续15秒后才触发
    resolve_timeout_ms: 30000
  })
  
  // 创建告警规则
  let high_cpu_rule = AlertRule({
    name: "HighCPUUsage",
    description: "CPU usage is above 80%",
    query: "avg(system.cpu.usage) > 0.8",
    severity: "warning",
    for_duration_ms: 10000,
    labels: [("team", "infrastructure"), ("service", "system")],
    annotations: [("summary", "High CPU usage detected"), ("runbook_url", "https://runbooks.example.com/cpu")]
  })
  
  let critical_memory_rule = AlertRule({
    name: "CriticalMemoryUsage",
    description: "Memory usage is above 95%",
    query: "system.memory.usage > 0.95",
    severity: "critical",
    for_duration_ms: 5000,
    labels: [("team", "infrastructure"), ("service", "system")],
    annotations = [("summary", "Critical memory usage detected"), ("action", "immediate investigation required")]
  })
  
  let error_rate_rule = AlertRule({
    name: "HighErrorRate",
    description: "Error rate is above 5%",
    query: "rate(http.requests.error) / rate(http.requests.total) > 0.05",
    severity: "warning",
    for_duration_ms: 15000,
    labels: [("team", "backend"), ("service", "api")],
    annotations = [("summary", "High error rate detected")]
  })
  
  // 注册告警规则
  alert_manager.register_rule(high_cpu_rule)
  alert_manager.register_rule(critical_memory_rule)
  alert_manager.register_rule(error_rate_rule)
  
  // 创建告警通知渠道
  let email_channel = NotificationChannel({
    name: "email",
    type: "email",
    enabled: true,
    settings: {
      "smtp_server": "smtp.example.com",
      "from": "alerts@example.com",
      "to": ["team@example.com"]
    }
  })
  
  let slack_channel = NotificationChannel({
    name: "slack",
    type: "slack",
    enabled: true,
    settings: {
      "webhook_url": "https://hooks.slack.com/services/...",
      "channel": "#alerts"
    }
  })
  
  let pagerduty_channel = NotificationChannel({
    name: "pagerduty",
    type: "pagerduty",
    enabled: true,
    settings: {
      "service_key": "pd-service-key",
      "severity": "high"
    }
  })
  
  // 注册通知渠道
  alert_manager.register_channel(email_channel)
  alert_manager.register_channel(slack_channel)
  alert_manager.register_channel(pagerduty_channel)
  
  // 配置告警路由
  alert_manager.add_route(Route({
    matchers = [("severity", "critical")],
    channels = ["pagerduty", "slack", "email"],
    group_by = ["team", "service"]
  }))
  
  alert_manager.add_route(Route({
    matchers = [("severity", "warning")],
    channels = ["slack", "email"],
    group_by = ["team"]
  }))
  
  // 模拟度量数据
  let metrics_store = MetricsStore()
  
  // 添加高CPU使用率数据（触发告警）
  let base_time = get_current_timestamp()
  for i in 0..20 {
    let timestamp = base_time + (i * 1000)  // 每秒一个数据点
    let value = if i < 10 { 0.6 } else { 0.85 }  // 前10秒正常，后10秒高CPU
    metrics_store.add_metric("system.cpu.usage", value, timestamp, [("host", "server-01")])
  }
  
  // 添加关键内存使用率数据（触发告警）
  for i in 0..20 {
    let timestamp = base_time + (i * 1000)
    let value = if i < 5 { 0.8 } else { 0.97 }  // 前5秒正常，后15秒高内存
    metrics_store.add_metric("system.memory.usage", value, timestamp, [("host", "server-01")])
  }
  
  // 添加错误率数据（不触发告警，因为持续时间不够）
  for i in 0..10 {
    let timestamp = base_time + (i * 1000)
    let total_requests = 100.0
    let error_requests = if i < 5 { 2.0 } else { 8.0 }  // 前5秒2%错误率，后5秒8%错误率
    metrics_store.add_metric("http.requests.total", total_requests, timestamp, [("service", "api")])
    metrics_store.add_metric("http.requests.error", error_requests, timestamp, [("service", "api")])
  }
  
  // 评估告警规则
  let evaluation_results = alert_manager.evaluate_rules(metrics_store, base_time + 20000)
  
  // 验证告警结果
  assert_eq(evaluation_results.length(), 2)  // 两个告警应该触发
  
  // 验证高CPU告警
  let high_cpu_alert = evaluation_results.find(fn(result) { result.rule_name == "HighCPUUsage" })
  match high_cpu_alert {
    Some(alert) => {
      assert_eq(alert.status, "firing")
      assert_eq(alert.severity, "warning")
      assert_true(alert.labels.contains(("team", "infrastructure")))
      assert_true(alert.annotations.contains(("summary", "High CPU usage detected")))
    }
    None => assert_true(false)
  }
  
  // 验证关键内存告警
  let critical_memory_alert = evaluation_results.find(fn(result) { result.rule_name == "CriticalMemoryUsage" })
  match critical_memory_alert {
    Some(alert) => {
      assert_eq(alert.status, "firing")
      assert_eq(alert.severity, "critical")
      assert_true(alert.labels.contains(("team", "infrastructure")))
      assert_true(alert.annotations.contains(("action", "immediate investigation required")))
    }
    None => assert_true(false)
  }
  
  // 验证错误率告警未触发（持续时间不够）
  let error_rate_alert = evaluation_results.find(fn(result) { result.rule_name == "HighErrorRate" })
  assert_eq(error_rate_alert, None)
  
  // 测试告警通知
  let notifications = alert_manager.send_notifications(evaluation_results)
  
  // 验证通知发送
  assert_true(notifications.length() > 0)
  
  // 高CPU告警应该发送到slack和email
  let high_cpu_notifications = notifications.filter(fn(n) { n.alert_id == high_cpu_alert!.id })
  assert_true(high_cpu_notifications.length() >= 2)  // 至少slack和email
  
  // 关键内存告警应该发送到所有渠道
  let critical_memory_notifications = notifications.filter(fn(n) { n.alert_id == critical_memory_alert!.id })
  assert_true(critical_memory_notifications.length() >= 3)  // pagerduty、slack和email
  
  // 测试告警恢复
  // 添加恢复数据
  for i in 20..30 {
    let timestamp = base_time + (i * 1000)
    metrics_store.add_metric("system.cpu.usage", 0.5, timestamp, [("host", "server-01")])  // 恢复正常
    metrics_store.add_metric("system.memory.usage", 0.7, timestamp, [("host", "server-01")])  // 恢复正常
  }
  
  // 重新评估告警
  let recovery_results = alert_manager.evaluate_rules(metrics_store, base_time + 35000)
  
  // 验证告警恢复
  let recovered_high_cpu = recovery_results.find(fn(result) { result.rule_name == "HighCPUUsage" })
  let recovered_critical_memory = recovery_results.find(fn(result) { result.rule_name == "CriticalMemoryUsage" })
  
  match recovered_high_cpu {
    Some(alert) => assert_eq(alert.status, "resolved")
    None => assert_true(false)
  }
  
  match recovered_critical_memory {
    Some(alert) => assert_eq(alert.status, "resolved")
    None => assert_true(false)
  }
}

// 测试3: 实时性能监控
test "实时性能监控" {
  // 创建性能监控器
  let performance_monitor = PerformanceMonitor({
    sampling_interval_ms: 1000,
    metrics_retention_ms: 3600000,  // 保留1小时
    aggregation_windows: [10000, 60000, 300000]  // 10秒、1分钟、5分钟
  })
  
  // 监控应用程序性能指标
  let app_metrics = [
    ("response_time", "gauge"),
    ("throughput", "counter"),
    ("error_rate", "gauge"),
    ("cpu_usage", "gauge"),
    ("memory_usage", "gauge"),
    ("active_connections", "gauge"),
    ("queue_depth", "gauge"),
    ("gc_pause_time", "histogram")
  ]
  
  // 注册性能指标
  for metric in app_metrics {
    performance_monitor.register_metric(metric.0, metric.1)
  }
  
  // 模拟应用程序性能数据
  let base_time = get_current_timestamp()
  
  for i in 0..300 {  // 5分钟的数据
    let timestamp = base_time + (i * 1000)
    
    // 响应时间（有波动）
    let response_time = 50.0 + (Random::next() * 100.0) + (i % 60 == 0 ? 200.0 : 0.0)
    performance_monitor.record_gauge("response_time", response_time, timestamp)
    
    // 吞吐量（相对稳定，偶有峰值）
    let throughput = 1000.0 + (Random::next() * 200.0) + (i % 120 == 0 ? 500.0 : 0.0)
    performance_monitor.record_counter("throughput", throughput, timestamp)
    
    // 错误率（大部分时间很低，偶有飙升）
    let error_rate = (Random::next() * 2.0) + (i % 180 == 0 ? 10.0 : 0.0)
    performance_monitor.record_gauge("error_rate", error_rate, timestamp)
    
    // CPU使用率
    let cpu_usage = 30.0 + (Random::next() * 40.0)
    performance_monitor.record_gauge("cpu_usage", cpu_usage, timestamp)
    
    // 内存使用率（逐渐增长，偶有下降）
    let memory_usage = 50.0 + (i * 0.1) % 30.0 + (Random::next() * 10.0)
    performance_monitor.record_gauge("memory_usage", memory_usage, timestamp)
    
    // 活跃连接数
    let active_connections = 50.0 + (Random::next() * 50.0)
    performance_monitor.record_gauge("active_connections", active_connections, timestamp)
    
    // 队列深度
    let queue_depth = (Random::next() * 20.0) + (i % 90 == 0 ? 50.0 : 0.0)
    performance_monitor.record_gauge("queue_depth", queue_depth, timestamp)
    
    // GC暂停时间
    let gc_pause = 10.0 + (Random::next() * 20.0)
    performance_monitor.record_histogram("gc_pause_time", gc_pause, timestamp)
  }
  
  // 测试实时性能分析
  let current_time = base_time + 300000  // 5分钟后
  
  // 获取最近1分钟的性能指标
  let recent_metrics = performance_monitor.get_metrics(current_time, 60000)
  
  // 验证指标收集
  assert_true(recent_metrics.contains("response_time"))
  assert_true(recent_metrics.contains("throughput"))
  assert_true(recent_metrics.contains("error_rate"))
  assert_true(recent_metrics.contains("cpu_usage"))
  assert_true(recent_metrics.contains("memory_usage"))
  assert_true(recent_metrics.contains("active_connections"))
  assert_true(recent_metrics.contains("queue_depth"))
  assert_true(recent_metrics.contains("gc_pause_time"))
  
  // 验证响应时间分析
  let response_time_data = recent_metrics.get("response_time")
  match response_time_data {
    Some(data) => {
      assert_true(data.count > 0)
      assert_true(data.average > 0)
      assert_true(data.p95 > 0)
      assert_true(data.p99 > 0)
      assert_true(data.max >= data.p95)
      assert_true(data.p95 >= data.average)
    }
    None => assert_true(false)
  }
  
  // 验证吞吐量分析
  let throughput_data = recent_metrics.get("throughput")
  match throughput_data {
    Some(data) => {
      assert_true(data.count > 0)
      assert_true(data.sum > 0)
      assert_true(data.rate > 0)  // 每秒速率
    }
    None => assert_true(false)
  }
  
  // 测试性能异常检测
  let anomaly_detector = AnomalyDetector({
    algorithm: "statistical",
    sensitivity: 0.95,
    window_size: 60  // 1分钟窗口
  })
  
  // 检测响应时间异常
  let response_time_anomalies = anomaly_detector.detect_anomalies(
    performance_monitor.get_time_series("response_time", current_time - 300000, current_time)
  )
  
  // 验证异常检测
  assert_true(response_time_anomalies.length() > 0)  // 应该检测到一些异常（模拟的峰值）
  
  for anomaly in response_time_anomalies {
    assert_true(anomaly.severity > 0.0)
    assert_true(anomaly.severity <= 1.0)
    assert_true(anomaly.timestamp > 0)
    assert_eq(anomaly.metric_name, "response_time")
  }
  
  // 测试性能趋势分析
  let trend_analyzer = TrendAnalyzer({
    window_size: 300,  // 5分钟窗口
    min_data_points: 60  // 至少1分钟数据
  })
  
  // 分析响应时间趋势
  let response_time_trend = trend_analyzer.analyze_trend(
    performance_monitor.get_time_series("response_time", current_time - 300000, current_time)
  )
  
  // 验证趋势分析
  match response_time_trend {
    Some(trend) => {
      assert_true(trend.direction == "increasing" || trend.direction == "decreasing" || trend.direction == "stable")
      assert_true(trend.slope >= -1.0 && trend.slope <= 1.0)
      assert_true(trend.confidence >= 0.0 && trend.confidence <= 1.0)
    }
    None => assert_true(false)
  }
  
  // 测试性能预测
  let performance_predictor = PerformancePredictor({
    algorithm: "linear_regression",
    prediction_horizon_ms: 300000,  // 预测未来5分钟
    training_window_ms: 1800000     // 使用过去30分钟数据训练
  })
  
  // 预测内存使用率
  let memory_prediction = performance_predictor.predict(
    performance_monitor.get_time_series("memory_usage", current_time - 1800000, current_time),
    current_time + 300000
  )
  
  // 验证预测结果
  match memory_prediction {
    Some(prediction) => {
      assert_true(prediction.predicted_value > 0)
      assert_true(prediction.confidence_interval_lower > 0)
      assert_true(prediction.confidence_interval_upper > prediction.confidence_interval_lower)
      assert_true(prediction.confidence >= 0.0 && prediction.confidence <= 1.0)
    }
    None => assert_true(false)
  }
  
  // 测试性能报告生成
  let report_generator = PerformanceReportGenerator({
    include_charts: true,
    include_anomalies: true,
    include_predictions: true,
    format: "html"
  })
  
  let performance_report = report_generator.generate_report(
    performance_monitor,
    current_time - 300000,  // 过去5分钟
    current_time
  )
  
  // 验证报告内容
  assert_true(performance_report.contains("Performance Report"))
  assert_true(performance_report.contains("Response Time"))
  assert_true(performance_report.contains("Throughput"))
  assert_true(performance_report.contains("Error Rate"))
  assert_true(performance_report.contains("CPU Usage"))
  assert_true(performance_report.contains("Memory Usage"))
  
  if response_time_anomalies.length() > 0 {
    assert_true(performance_report.contains("Anomalies"))
  }
  
  if memory_prediction != None {
    assert_true(performance_report.contains("Predictions"))
  }
}

// 测试4: 实时拓扑监控
test "实时拓扑监控" {
  // 创建拓扑监控器
  let topology_monitor = TopologyMonitor({
    discovery_interval_ms: 30000,  // 30秒发现一次
    refresh_interval_ms: 10000,    // 10秒刷新一次
    health_check_interval_ms: 5000  // 5秒健康检查一次
  })
  
  // 定义服务拓扑
  let service_topology = Topology({
    name: "microservices",
    nodes: [
      Node({
        id: "api-gateway",
        type: "service",
        name: "API Gateway",
        version: "1.2.3",
        health_endpoint: "http://api-gateway:8080/health",
        metrics_endpoint: "http://api-gateway:8080/metrics"
      }),
      Node({
        id: "user-service",
        type: "service",
        name: "User Service",
        version: "2.1.0",
        health_endpoint: "http://user-service:8081/health",
        metrics_endpoint: "http://user-service:8081/metrics"
      }),
      Node({
        id: "order-service",
        type: "service",
        name: "Order Service",
        version: "1.5.2",
        health_endpoint: "http://order-service:8082/health",
        metrics_endpoint: "http://order-service:8082/metrics"
      }),
      Node({
        id: "payment-service",
        type: "service",
        name: "Payment Service",
        version: "3.0.1",
        health_endpoint: "http://payment-service:8083/health",
        metrics_endpoint: "http://payment-service:8083/metrics"
      }),
      Node({
        id: "notification-service",
        type: "service",
        name: "Notification Service",
        version: "1.0.5",
        health_endpoint: "http://notification-service:8084/health",
        metrics_endpoint: "http://notification-service:8084/metrics"
      }),
      Node({
        id: "user-db",
        type: "database",
        name: "User Database",
        version: "PostgreSQL 13",
        health_endpoint: "http://user-db:5432/health"
      }),
      Node({
        id: "order-db",
        type: "database",
        name: "Order Database",
        version: "PostgreSQL 13",
        health_endpoint: "http://order-db:5432/health"
      }),
      Node({
        id: "payment-db",
        type: "database",
        name: "Payment Database",
        version: "PostgreSQL 13",
        health_endpoint: "http://payment-db:5432/health"
      }),
      Node({
        id: "redis-cache",
        type: "cache",
        name: "Redis Cache",
        version: "Redis 6.2",
        health_endpoint: "http://redis-cache:6379/health"
      })
    ],
    edges: [
      Edge({
        from: "api-gateway",
        to: "user-service",
        type: "http",
        protocol: "REST"
      }),
      Edge({
        from: "api-gateway",
        to: "order-service",
        type: "http",
        protocol: "REST"
      }),
      Edge({
        from: "user-service",
        to: "user-db",
        type: "database",
        protocol: "SQL"
      }),
      Edge({
        from: "order-service",
        to: "order-db",
        type: "database",
        protocol: "SQL"
      }),
      Edge({
        from: "order-service",
        to: "payment-service",
        type: "http",
        protocol: "REST"
      }),
      Edge({
        from: "payment-service",
        to: "payment-db",
        type: "database",
        protocol: "SQL"
      }),
      Edge({
        from: "order-service",
        to: "notification-service",
        type: "message",
        protocol: "AMQP"
      }),
      Edge({
        from: "user-service",
        to: "redis-cache",
        type: "cache",
        protocol: "Redis"
      }),
      Edge({
        from: "order-service",
        to: "redis-cache",
        type: "cache",
        protocol: "Redis"
      })
    ]
  })
  
  // 注册拓扑
  topology_monitor.register_topology(service_topology)
  
  // 模拟服务健康状态
  let health_status = [
    ("api-gateway", "healthy"),
    ("user-service", "healthy"),
    ("order-service", "degraded"),  // 部分功能降级
    ("payment-service", "healthy"),
    ("notification-service", "unhealthy"),  // 不健康
    ("user-db", "healthy"),
    ("order-db", "healthy"),
    ("payment-db", "healthy"),
    ("redis-cache", "healthy")
  ]
  
  // 设置健康状态
  for status in health_status {
    topology_monitor.set_node_health(status.0, status.1)
  }
  
  // 模拟服务间调用指标
  let base_time = get_current_timestamp()
  
  let call_metrics = [
    ("api-gateway", "user-service", 1000, 50),    // 1000次调用，平均50ms延迟
    ("api-gateway", "order-service", 800, 120),   // 800次调用，平均120ms延迟
    ("user-service", "user-db", 1500, 25),        // 1500次调用，平均25ms延迟
    ("order-service", "order-db", 600, 45),       // 600次调用，平均45ms延迟
    ("order-service", "payment-service", 200, 200), // 200次调用，平均200ms延迟
    ("payment-service", "payment-db", 250, 30),   // 250次调用，平均30ms延迟
    ("order-service", "notification-service", 300, 80), // 300次调用，平均80ms延迟
    ("user-service", "redis-cache", 2000, 5),     // 2000次调用，平均5ms延迟
    ("order-service", "redis-cache", 1200, 8)     // 1200次调用，平均8ms延迟
  ]
  
  // 设置调用指标
  for metric in call_metrics {
    topology_monitor.set_call_metrics(
      metric.0, metric.1, 
      metric.2, metric.3, 
      base_time
    )
  }
  
  // 获取拓扑快照
  let topology_snapshot = topology_monitor.get_snapshot()
  
  // 验证拓扑结构
  assert_eq(topology_snapshot.nodes.length(), 9)
  assert_eq(topology_snapshot.edges.length(), 9)
  
  // 验证节点健康状态
  let api_gateway = topology_snapshot.nodes.find(fn(n) { n.id == "api-gateway" })
  match api_gateway {
    Some(node) => assert_eq(node.health, "healthy")
    None => assert_true(false)
  }
  
  let order_service = topology_snapshot.nodes.find(fn(n) { n.id == "order-service" })
  match order_service {
    Some(node) => assert_eq(node.health, "degraded")
    None => assert_true(false)
  }
  
  let notification_service = topology_snapshot.nodes.find(fn(n) { n.id == "notification-service" })
  match notification_service {
    Some(node) => assert_eq(node.health, "unhealthy")
    None => assert_true(false)
  }
  
  // 验证调用指标
  let gateway_to_user = topology_snapshot.edges.find(fn(e) { e.from == "api-gateway" && e.to == "user-service" })
  match gateway_to_user {
    Some(edge) => {
      assert_eq(edge.call_count, 1000)
      assert_eq(edge.average_latency, 50)
    }
    None => assert_true(false)
  }
  
  // 测试拓扑分析
  let topology_analyzer = TopologyAnalyzer()
  
  // 分析关键路径
  let critical_paths = topology_analyzer.find_critical_paths(topology_snapshot)
  
  // 验证关键路径识别
  assert_true(critical_paths.length() > 0)
  
  // 应该识别出API Gateway到Order Service到Payment Service的路径
  let order_payment_path = critical_paths.find(fn(path) => {
    path.contains("api-gateway") && 
    path.contains("order-service") && 
    path.contains("payment-service")
  })
  
  assert_true(order_payment_path != None)
  
  // 分析单点故障
  let single_points_of_failure = topology_analyzer.find_single_points_of_failure(topology_snapshot)
  
  // 验证单点故障识别
  assert_true(single_points_of_failure.length() > 0)
  
  // API Gateway应该是单点故障
  assert_true(single_points_of_failure.contains("api-gateway"))
  
  // 分析性能瓶颈
  let performance_bottlenecks = topology_analyzer.find_performance_bottlenecks(topology_snapshot)
  
  // 验证性能瓶颈识别
  assert_true(performance_bottlenecks.length() > 0)
  
  // 高延迟的连接应该是瓶颈
  let order_to_payment = performance_bottlenecks.find(fn(b) => {
    b.from == "order-service" && b.to == "payment-service"
  })
  
  assert_true(order_to_payment != None)
  
  // 测试拓扑变化检测
  let previous_snapshot = topology_monitor.get_historical_snapshot(base_time - 60000)  // 1分钟前
  
  // 模拟拓扑变化
  let updated_topology = Topology({
    name = "microservices",
    nodes = service_topology.nodes + [
      Node({
        id: "inventory-service",
        type: "service",
        name: "Inventory Service",
        version: "1.0.0",
        health_endpoint: "http://inventory-service:8085/health",
        metrics_endpoint: "http://inventory-service:8085/metrics"
      })
    ],
    edges = service_topology.edges + [
      Edge({
        from: "order-service",
        to: "inventory-service",
        type: "http",
        protocol: "REST"
      })
    ]
  })
  
  topology_monitor.update_topology(updated_topology)
  
  // 检测拓扑变化
  let topology_changes = topology_monitor.detect_changes(previous_snapshot, topology_snapshot)
  
  // 验证变化检测
  assert_true(topology_changes.length() > 0)
  
  // 应该检测到新增的inventory-service
  let added_nodes = topology_changes.filter(fn(change) => change.type == "node_added")
  assert_true(added_nodes.length() > 0)
  
  let inventory_added = added_nodes.find(fn(change) => change.node_id == "inventory-service")
  assert_true(inventory_added != None)
  
  // 测试拓扑可视化数据生成
  let visualization_generator = TopologyVisualizationGenerator({
    layout: "force_directed",
    node_size_by: "connections",
    edge_width_by: "traffic",
    color_by: "health"
  })
  
  let visualization_data = visualization_generator.generate(topology_snapshot)
  
  // 验证可视化数据
  assert_true(visualization_data.contains("nodes"))
  assert_true(visualization_data.contains("edges"))
  assert_true(visualization_data.contains("api-gateway"))
  assert_true(visualization_data.contains("user-service"))
  
  // 验证健康状态颜色映射
  assert_true(visualization_data.contains("\"healthy\"") || visualization_data.contains("green"))
  assert_true(visualization_data.contains("\"degraded\"") || visualization_data.contains("yellow"))
  assert_true(visualization_data.contains("\"unhealthy\"") || visualization_data.contains("red"))
}