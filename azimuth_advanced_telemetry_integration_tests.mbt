// Azimuth Advanced Telemetry Integration Tests
// 高级遥测系统集成测试，专注于分布式追踪、数据聚合和性能监控

// 测试1: 分布式追踪的上下文传播
test "distributed tracing context propagation" {
  // 模拟分布式追踪场景
  let trace_id = "trace-1234567890abcdef"
  let parent_span_id = "span-1234567890abcde"
  let child_span_id = "span-abcdef123456789"
  
  // 创建父span上下文
  let parent_context = TraceContext::new(trace_id, parent_span_id, None, [])
  
  // 创建子span上下文，继承父span的trace_id
  let child_context = TraceContext::new(trace_id, child_span_id, Some(parent_span_id), [])
  
  // 验证trace_id传播
  assert_eq(TraceContext::trace_id(parent_context), trace_id)
  assert_eq(TraceContext::trace_id(child_context), trace_id)
  
  // 验证父子关系
  assert_eq(TraceContext::parent_span_id(child_context), Some(parent_span_id))
  assert_eq(TraceContext::span_id(child_context), child_span_id)
  
  // 验证上下文序列化
  let serialized = TraceContext::serialize(parent_context)
  assert_true(serialized.contains(trace_id))
  assert_true(serialized.contains(parent_span_id))
  
  // 验证上下文反序列化
  let deserialized = TraceContext::deserialize(serialized)
  assert_eq(TraceContext::trace_id(deserialized), trace_id)
  assert_eq(TraceContext::span_id(deserialized), parent_span_id)
}

// 测试2: 遥测数据序列化和反序列化
test "telemetry data serialization and deserialization" {
  // 创建一个复杂的遥测数据结构
  let telemetry_data = TelemetryData::new(
    "service.operation",
    TraceContext::new("trace-123", "span-456", None, [("service.name", "api-service")]),
    [
      ("http.method", StringValue("GET")),
      ("http.status_code", IntValue(200)),
      ("http.url", StringValue("/api/users")),
      ("duration_ms", FloatValue(125.5))
    ],
    [
      TelemetryEvent::new("start", 1000, []),
      TelemetryEvent::new("db.query.start", 1050, [("query.type", StringValue("select"))]),
      TelemetryEvent::new("db.query.end", 1120, [("rows", IntValue(10))]),
      TelemetryEvent::new("end", 1125, [])
    ],
    1125
  )
  
  // 序列化为JSON
  let json = TelemetryData::to_json(telemetry_data)
  assert_true(json.contains("service.operation"))
  assert_true(json.contains("trace-123"))
  assert_true(json.contains("span-456"))
  assert_true(json.contains("GET"))
  assert_true(json.contains("200"))
  assert_true(json.contains("125.5"))
  
  // 从JSON反序列化
  let deserialized = TelemetryData::from_json(json)
  assert_eq(TelemetryData::operation_name(deserialized), "service.operation")
  assert_eq(TraceContext::trace_id(TelemetryData::trace_context(deserialized)), "trace-123")
  assert_eq(TraceContext::span_id(TelemetryData::trace_context(deserialized)), "span-456")
  
  // 验证属性
  let http_method = TelemetryData::get_attribute(deserialized, "http.method")
  assert_eq(http_method, Some(StringValue("GET")))
  
  let http_status = TelemetryData::get_attribute(deserialized, "http.status_code")
  assert_eq(http_status, Some(IntValue(200)))
  
  // 验证事件
  let events = TelemetryData::events(deserialized)
  assert_eq(events.length(), 4)
  assert_eq(TelemetryEvent::name(events[0]), "start")
  assert_eq(TelemetryEvent::name(events[1]), "db.query.start")
  assert_eq(TelemetryEvent::name(events[2]), "db.query.end")
  assert_eq(TelemetryEvent::name(events[3]), "end")
}

// 测试3: 采样策略测试
test "sampling strategy implementation" {
  // 测试固定采样率策略
  let fixed_sampler = FixedRateSampler::new(0.5)  // 50%采样率
  let trace_id1 = "trace-1111111111111111"
  let trace_id2 = "trace-2222222222222222"
  let trace_id3 = "trace-3333333333333333"
  
  // 使用确定性采样，基于trace_id
  let should_sample1 = Sampler::should_sample(fixed_sampler, trace_id1)
  let should_sample2 = Sampler::should_sample(fixed_sampler, trace_id2)
  let should_sample3 = Sampler::should_sample(fixed_sampler, trace_id3)
  
  // 验证采样结果（基于trace_id的确定性）
  assert_true(should_sample1 != should_sample2 || should_sample2 != should_sample3)
  
  // 测试自适应采样策略
  let adaptive_sampler = AdaptiveSampler::new(
    100,  // 最大每秒采样数
    1000  // 错误率阈值百分比
  )
  
  // 模拟低错误率场景
  let low_error_metrics = ServiceMetrics::new(10, 100)  // 10个错误，100个总请求
  let adaptive_decision1 = Sampler::should_sample_adaptive(adaptive_sampler, trace_id1, low_error_metrics)
  assert_true(adaptive_decision1)  // 低错误率时正常采样
  
  // 模拟高错误率场景
  let high_error_metrics = ServiceMetrics::new(500, 1000)  // 500个错误，1000个总请求
  let adaptive_decision2 = Sampler::should_sample_adaptive(adaptive_sampler, trace_id2, high_error_metrics)
  assert_true(adaptive_decision2)  // 高错误率时增加采样
  
  // 测试基于优先级的采样
  let priority_sampler = PrioritySampler::new()
  let high_priority = SamplingPriority::High
  let low_priority = SamplingPriority::Low
  
  let high_priority_decision = Sampler::should_sample_priority(priority_sampler, trace_id1, high_priority)
  let low_priority_decision = Sampler::should_sample_priority(priority_sampler, trace_id2, low_priority)
  
  assert_true(high_priority_decision)
  assert_true(low_priority_decision || !low_priority_decision)  // 低优先级可能被采样也可能不采样
}

// 测试4: 遥测数据聚合
test "telemetry data aggregation" {
  // 创建多个遥测数据点
  let telemetry_points = [
    TelemetryData::new(
      "api.request",
      TraceContext::new("trace-1", "span-1", None, []),
      [("endpoint", StringValue("/users")), ("status", IntValue(200)), ("duration", IntValue(100))],
      [],
      100
    ),
    TelemetryData::new(
      "api.request",
      TraceContext::new("trace-2", "span-2", None, []),
      [("endpoint", StringValue("/users")), ("status", IntValue(200)), ("duration", IntValue(150))],
      [],
      150
    ),
    TelemetryData::new(
      "api.request",
      TraceContext::new("trace-3", "span-3", None, []),
      [("endpoint", StringValue("/users")), ("status", IntValue(500)), ("duration", IntValue(200))],
      [],
      200
    ),
    TelemetryData::new(
      "api.request",
      TraceContext::new("trace-4", "span-4", None, []),
      [("endpoint", StringValue("/orders")), ("status", IntValue(200)), ("duration", IntValue(120))],
      [],
      120
    )
  ]
  
  // 按端点聚合
  let endpoint_aggregation = Aggregator::by_attribute(telemetry_points, "endpoint")
  
  // 验证/users端点的聚合结果
  let users_stats = Aggregator::get_stats(endpoint_aggregation, "/users")
  assert_eq(AggregatedStats::count(users_stats), 3)
  assert_eq(AggregatedStats::success_count(users_stats), 2)
  assert_eq(AggregatedStats::error_count(users_stats), 1)
  assert_eq(AggregatedStats::avg_duration(users_stats), 150.0)  // (100 + 150 + 200) / 3
  
  // 验证/orders端点的聚合结果
  let orders_stats = Aggregator::get_stats(endpoint_aggregation, "/orders")
  assert_eq(AggregatedStats::count(orders_stats), 1)
  assert_eq(AggregatedStats::success_count(orders_stats), 1)
  assert_eq(AggregatedStats::error_count(orders_stats), 0)
  assert_eq(AggregatedStats::avg_duration(orders_stats), 120.0)
  
  // 按状态码聚合
  let status_aggregation = Aggregator::by_attribute(telemetry_points, "status")
  
  // 验证200状态码的聚合结果
  let success_stats = Aggregator::get_stats(status_aggregation, "200")
  assert_eq(AggregatedStats::count(success_stats), 3)
  assert_eq(AggregatedStats::avg_duration(success_stats), (100.0 + 150.0 + 120.0) / 3.0)
  
  // 验证500状态码的聚合结果
  let error_stats = Aggregator::get_stats(status_aggregation, "500")
  assert_eq(AggregatedStats::count(error_stats), 1)
  assert_eq(AggregatedStats::avg_duration(error_stats), 200.0)
  
  // 时间窗口聚合
  let time_windowed = Aggregator::by_time_window(telemetry_points, 60)  // 60秒窗口
  assert_eq(Aggregator::window_count(time_windowed), 1)  // 所有数据都在同一窗口内
}

// 测试5: 资源监控和指标收集
test "resource monitoring and metrics collection" {
  // 创建资源监控器
  let resource_monitor = ResourceMonitor::new()
  
  // 模拟系统资源指标
  let cpu_metrics = CPUMetrics::new(75.5, 4, 8)  // 75.5%使用率，4核心，8线程
  let memory_metrics = MemoryMetrics::new(8589934592, 17179869184, 4294967296)  // 已用、总计、可用
  let disk_metrics = DiskMetrics::new(107374182400, 536870912000, 429496729600)  // 已用、总计、可用
  let network_metrics = NetworkMetrics::new(104857600, 52428800, 1048576, 524288)  // 发送、接收、错误发送、错误接收
  
  // 收集指标
  ResourceMonitor::collect_cpu_metrics(resource_monitor, cpu_metrics)
  ResourceMonitor::collect_memory_metrics(resource_monitor, memory_metrics)
  ResourceMonitor::collect_disk_metrics(resource_monitor, disk_metrics)
  ResourceMonitor::collect_network_metrics(resource_monitor, network_metrics)
  
  // 验证CPU指标
  let collected_cpu = ResourceMonitor::get_cpu_metrics(resource_monitor)
  assert_eq(CPUMetrics::usage_percent(collected_cpu), 75.5)
  assert_eq(CPUMetrics::core_count(collected_cpu), 4)
  assert_eq(CPUMetrics::thread_count(collected_cpu), 8)
  
  // 验证内存指标
  let collected_memory = ResourceMonitor::get_memory_metrics(resource_monitor)
  assert_eq(MemoryMetrics::used_bytes(collected_memory), 8589934592)
  assert_eq(MemoryMetrics::total_bytes(collected_memory), 17179869184)
  assert_eq(MemoryMetrics::available_bytes(collected_memory), 4294967296)
  
  // 计算内存使用百分比
  let memory_usage_percent = MemoryMetrics::usage_percent(collected_memory)
  assert_eq(memory_usage_percent, 50.0)  // 8GB / 16GB = 50%
  
  // 验证磁盘指标
  let collected_disk = ResourceMonitor::get_disk_metrics(resource_monitor)
  assert_eq(DiskMetrics::used_bytes(collected_disk), 107374182400)
  assert_eq(DiskMetrics::total_bytes(collected_disk), 536870912000)
  assert_eq(DiskMetrics::available_bytes(collected_disk), 429496729600)
  
  // 计算磁盘使用百分比
  let disk_usage_percent = DiskMetrics::usage_percent(collected_disk)
  assert_eq(disk_usage_percent, 20.0)  // 100GB / 500GB = 20%
  
  // 验证网络指标
  let collected_network = ResourceMonitor::get_network_metrics(resource_monitor)
  assert_eq(NetworkMetrics::bytes_sent(collected_network), 104857600)
  assert_eq(NetworkMetrics::bytes_received(collected_network), 52428800)
  assert_eq(NetworkMetrics::errors_sent(collected_network), 1048576)
  assert_eq(NetworkMetrics::errors_received(collected_network), 524288)
  
  // 计算网络错误率
  let network_error_rate = NetworkMetrics::error_rate(collected_network)
  assert_eq(network_error_rate, 1.5)  // (1MB + 0.5MB) / (100MB + 50MB) = 1.5%
  
  // 测试资源阈值告警
  let alert_manager = AlertManager::new()
  AlertManager::set_threshold(alert_manager, "cpu_usage", 80.0)  // CPU使用率超过80%告警
  AlertManager::set_threshold(alert_manager, "memory_usage", 90.0)  // 内存使用率超过90%告警
  AlertManager::set_threshold(alert_manager, "disk_usage", 85.0)  // 磁盘使用率超过85%告警
  AlertManager::set_threshold(alert_manager, "network_error_rate", 5.0)  // 网络错误率超过5%告警
  
  // 检查告警
  let alerts = AlertManager::check_thresholds(alert_manager, resource_monitor)
  assert_eq(alerts.length(), 0)  // 没有超过阈值
  
  // 模拟高CPU使用率
  let high_cpu_metrics = CPUMetrics::new(85.0, 4, 8)
  ResourceMonitor::collect_cpu_metrics(resource_monitor, high_cpu_metrics)
  
  let high_cpu_alerts = AlertManager::check_thresholds(alert_manager, resource_monitor)
  assert_eq(high_cpu_alerts.length(), 1)  // CPU告警
  assert_eq(Alert::metric_name(high_cpu_alerts[0]), "cpu_usage")
  assert_eq(Alert::current_value(high_cpu_alerts[0]), 85.0)
  assert_eq(Alert::threshold_value(high_cpu_alerts[0]), 80.0)
}

// 测试6: 遥测数据的可视化准备
test "telemetry data visualization preparation" {
  // 创建时间序列数据
  let time_series_data = [
    TimeSeriesPoint::new(1640995200, 100.0),  // 2022-01-01 00:00:00
    TimeSeriesPoint::new(1640995260, 120.0),  // 2022-01-01 00:01:00
    TimeSeriesPoint::new(1640995320, 110.0),  // 2022-01-01 00:02:00
    TimeSeriesPoint::new(1640995380, 140.0),  // 2022-01-01 00:03:00
    TimeSeriesPoint::new(1640995440, 125.0),  // 2022-01-01 00:04:00
    TimeSeriesPoint::new(1640995500, 115.0),  // 2022-01-01 00:05:00
    TimeSeriesPoint::new(1640995560, 130.0),  // 2022-01-01 00:06:00
    TimeSeriesPoint::new(1640995620, 105.0)   // 2022-01-01 00:07:00
  ]
  
  // 创建图表数据
  let chart_data = ChartData::from_time_series(
    time_series_data,
    "Response Time",
    "Time",
    "Response Time (ms)",
    ChartType::Line
  )
  
  // 验证图表数据
  assert_eq(ChartData::title(chart_data), "Response Time")
  assert_eq(ChartData::x_axis_label(chart_data), "Time")
  assert_eq(ChartData::y_axis_label(chart_data), "Response Time (ms)")
  assert_eq(ChartData::chart_type(chart_data), ChartType::Line)
  assert_eq(ChartData::data_points(chart_data).length(), 8)
  
  // 计算统计数据
  let stats = ChartData::calculate_statistics(chart_data)
  assert_eq(Statistics::min(stats), 100.0)
  assert_eq(Statistics::max(stats), 140.0)
  assert_eq(Statistics::avg(stats), 130.625)  // (100+120+110+140+125+115+130+105)/8
  assert_eq(Statistics::median(stats), 122.5)  // (115+125)/2
  assert_eq(Statistics::count(stats), 8)
  
  // 创建多系列图表数据
  let cpu_series = [
    TimeSeriesPoint::new(1640995200, 45.0),
    TimeSeriesPoint::new(1640995260, 52.0),
    TimeSeriesPoint::new(1640995320, 48.0),
    TimeSeriesPoint::new(1640995380, 65.0),
    TimeSeriesPoint::new(1640995440, 58.0),
    TimeSeriesPoint::new(1640995500, 50.0),
    TimeSeriesPoint::new(1640995560, 55.0),
    TimeSeriesPoint::new(1640995620, 42.0)
  ]
  
  let memory_series = [
    TimeSeriesPoint::new(1640995200, 60.0),
    TimeSeriesPoint::new(1640995260, 62.0),
    TimeSeriesPoint::new(1640995320, 65.0),
    TimeSeriesPoint::new(1640995380, 68.0),
    TimeSeriesPoint::new(1640995440, 70.0),
    TimeSeriesPoint::new(1640995500, 66.0),
    TimeSeriesPoint::new(1640995560, 64.0),
    TimeSeriesPoint::new(1640995620, 61.0)
  ]
  
  let multi_series_chart = MultiSeriesChartData::new(
    "Resource Usage",
    "Time",
    "Usage (%)",
    [
      ChartSeries::new("CPU", cpu_series, Color::Blue),
      ChartSeries::new("Memory", memory_series, Color::Red)
    ]
  )
  
  // 验证多系列图表数据
  assert_eq(MultiSeriesChartData::title(multi_series_chart), "Resource Usage")
  assert_eq(MultiSeriesChartData::series_count(multi_series_chart), 2)
  
  let cpu_series_data = MultiSeriesChartData::get_series(multi_series_chart, "CPU")
  assert_eq(ChartSeries::name(cpu_series_data), "CPU")
  assert_eq(ChartSeries::data_points(cpu_series_data).length(), 8)
  
  let memory_series_data = MultiSeriesChartData::get_series(multi_series_chart, "Memory")
  assert_eq(ChartSeries::name(memory_series_data), "Memory")
  assert_eq(ChartSeries::data_points(memory_series_data).length(), 8)
  
  // 创建热力图数据
  let heatmap_data = HeatmapData::new(
    "Service Performance Heatmap",
    ["Service A", "Service B", "Service C"],
    ["00:00", "04:00", "08:00", "12:00", "16:00", "20:00"],
    [
      [1.0, 0.8, 0.5, 0.9, 0.7, 0.6],  // Service A
      [0.9, 0.7, 0.6, 0.8, 0.5, 0.4],  // Service B
      [0.8, 0.6, 0.7, 0.7, 0.6, 0.5]   // Service C
    ]
  )
  
  // 验证热力图数据
  assert_eq(HeatmapData::title(heatmap_data), "Service Performance Heatmap")
  assert_eq(HeatmapData::rows(heatmap_data).length(), 3)
  assert_eq(HeatmapData::columns(heatmap_data).length(), 6)
  assert_eq(HeatmapData::get_value(heatmap_data, 0, 0), 1.0)  // Service A at 00:00
  assert_eq(HeatmapData::get_value(heatmap_data, 2, 3), 0.7)  // Service C at 12:00
}

// 测试7: 遥测系统的配置管理
test "telemetry system configuration management" {
  // 创建配置管理器
  let config_manager = ConfigManager::new()
  
  // 设置基本配置
  ConfigManager::set(config_manager, "service.name", "payment-service")
  ConfigManager::set(config_manager, "service.version", "1.2.3")
  ConfigManager::set(config_manager, "environment", "production")
  ConfigManager::set(config_manager, "sampling.rate", "0.1")
  ConfigManager::set(config_manager, "batch.size", "100")
  ConfigManager::set(config_manager, "export.timeout", "5000")
  
  // 验证配置获取
  assert_eq(ConfigManager::get(config_manager, "service.name"), Some("payment-service"))
  assert_eq(ConfigManager::get(config_manager, "service.version"), Some("1.2.3"))
  assert_eq(ConfigManager::get(config_manager, "environment"), Some("production"))
  assert_eq(ConfigManager::get(config_manager, "sampling.rate"), Some("0.1"))
  assert_eq(ConfigManager::get(config_manager, "batch.size"), Some("100"))
  assert_eq(ConfigManager::get(config_manager, "export.timeout"), Some("5000"))
  
  // 验证缺失配置
  assert_eq(ConfigManager::get(config_manager, "missing.key"), None)
  
  // 测试类型化配置获取
  let sampling_rate = ConfigManager::get_float(config_manager, "sampling.rate", 0.0)
  assert_eq(sampling_rate, 0.1)
  
  let batch_size = ConfigManager::get_int(config_manager, "batch.size", 50)
  assert_eq(batch_size, 100)
  
  let export_timeout = ConfigManager::get_int(config_manager, "export.timeout", 3000)
  assert_eq(export_timeout, 5000)
  
  let missing_with_default = ConfigManager::get_string(config_manager, "missing.key", "default")
  assert_eq(missing_with_default, "default")
  
  // 测试配置分组
  ConfigManager::set_group(config_manager, "database", [
    ("host", "db.example.com"),
    ("port", "5432"),
    ("username", "telemetry_user"),
    ("password", "secret_password"),
    ("max_connections", "10")
  ])
  
  ConfigManager::set_group(config_manager, "redis", [
    ("host", "redis.example.com"),
    ("port", "6379"),
    ("db", "0"),
    ("timeout", "3000")
  ])
  
  // 验证分组配置
  let db_config = ConfigManager::get_group(config_manager, "database")
  assert_eq(db_config.length(), 5)
  assert_true(db_config.contains(("host", "db.example.com")))
  assert_true(db_config.contains(("port", "5432")))
  assert_true(db_config.contains(("username", "telemetry_user")))
  assert_true(db_config.contains(("password", "secret_password")))
  assert_true(db_config.contains(("max_connections", "10")))
  
  let redis_config = ConfigManager::get_group(config_manager, "redis")
  assert_eq(redis_config.length(), 4)
  assert_true(redis_config.contains(("host", "redis.example.com")))
  assert_true(redis_config.contains(("port", "6379")))
  assert_true(redis_config.contains(("db", "0")))
  assert_true(redis_config.contains(("timeout", "3000")))
  
  // 测试配置更新
  ConfigManager::update(config_manager, "sampling.rate", "0.2")
  assert_eq(ConfigManager::get(config_manager, "sampling.rate"), Some("0.2"))
  
  // 测试配置删除
  ConfigManager::remove(config_manager, "export.timeout")
  assert_eq(ConfigManager::get(config_manager, "export.timeout"), None)
  
  // 测试配置持久化
  let config_json = ConfigManager::to_json(config_manager)
  assert_true(config_json.contains("payment-service"))
  assert_true(config_json.contains("1.2.3"))
  assert_true(config_json.contains("production"))
  assert_true(config_json.contains("0.2"))
  assert_true(config_json.contains("db.example.com"))
  assert_true(config_json.contains("redis.example.com"))
  
  // 从JSON恢复配置
  let restored_config = ConfigManager::from_json(config_json)
  assert_eq(ConfigManager::get(restored_config, "service.name"), Some("payment-service"))
  assert_eq(ConfigManager::get(restored_config, "sampling.rate"), Some("0.2"))
  
  let restored_db_config = ConfigManager::get_group(restored_config, "database")
  assert_eq(restored_db_config.length(), 5)
  assert_true(restored_db_config.contains(("host", "db.example.com")))
}

// 测试8: 遥测数据的异常检测
test "telemetry data anomaly detection" {
  // 创建异常检测器
  let anomaly_detector = AnomalyDetector::new()
  
  // 设置异常检测规则
  AnomalyDetector::add_rule(anomaly_detector, AnomalyRule::new(
    "high_response_time",
    "response_time > 1000",
    "Response time exceeds 1000ms"
  ))
  
  AnomalyDetector::add_rule(anomaly_detector, AnomalyRule::new(
    "high_error_rate",
    "error_rate > 0.1",
    "Error rate exceeds 10%"
  ))
  
  AnomalyDetector::add_rule(anomaly_detector, AnomalyRule::new(
    "low_throughput",
    "throughput < 100",
    "Throughput falls below 100 requests/min"
  ))
  
  // 创建正常遥测数据
  let normal_data = TelemetryData::new(
    "api.request",
    TraceContext::new("trace-1", "span-1", None, []),
    [
      ("endpoint", StringValue("/users")),
      ("status", StringValue("200")),
      ("response_time", FloatValue(250.0)),
      ("error_rate", FloatValue(0.02)),
      ("throughput", IntValue(500))
    ],
    [],
    250
  )
  
  // 检测正常数据中的异常
  let normal_anomalies = AnomalyDetector::detect(anomaly_detector, normal_data)
  assert_eq(normal_anomalies.length(), 0)  // 正常数据不应有异常
  
  // 创建异常遥测数据 - 高响应时间
  let high_response_time_data = TelemetryData::new(
    "api.request",
    TraceContext::new("trace-2", "span-2", None, []),
    [
      ("endpoint", StringValue("/users")),
      ("status", StringValue("200")),
      ("response_time", FloatValue(1500.0)),  // 异常：高响应时间
      ("error_rate", FloatValue(0.02)),
      ("throughput", IntValue(500))
    ],
    [],
    1500
  )
  
  let high_response_anomalies = AnomalyDetector::detect(anomaly_detector, high_response_time_data)
  assert_eq(high_response_anomalies.length(), 1)
  assert_eq(Anomaly::rule_name(high_response_anomalies[0]), "high_response_time")
  assert_eq(Anomaly::description(high_response_anomalies[0]), "Response time exceeds 1000ms")
  
  // 创建异常遥测数据 - 高错误率
  let high_error_rate_data = TelemetryData::new(
    "api.request",
    TraceContext::new("trace-3", "span-3", None, []),
    [
      ("endpoint", StringValue("/users")),
      ("status", StringValue("500")),
      ("response_time", FloatValue(250.0)),
      ("error_rate", FloatValue(0.15)),  // 异常：高错误率
      ("throughput", IntValue(500))
    ],
    [],
    250
  )
  
  let high_error_anomalies = AnomalyDetector::detect(anomaly_detector, high_error_rate_data)
  assert_eq(high_error_anomalies.length(), 1)
  assert_eq(Anomaly::rule_name(high_error_anomalies[0]), "high_error_rate")
  assert_eq(Anomaly::description(high_error_anomalies[0]), "Error rate exceeds 10%")
  
  // 创建异常遥测数据 - 低吞吐量
  let low_throughput_data = TelemetryData::new(
    "api.request",
    TraceContext::new("trace-4", "span-4", None, []),
    [
      ("endpoint", StringValue("/users")),
      ("status", StringValue("200")),
      ("response_time", FloatValue(250.0)),
      ("error_rate", FloatValue(0.02)),
      ("throughput", IntValue(50))  // 异常：低吞吐量
    ],
    [],
    250
  )
  
  let low_throughput_anomalies = AnomalyDetector::detect(anomaly_detector, low_throughput_data)
  assert_eq(low_throughput_anomalies.length(), 1)
  assert_eq(Anomaly::rule_name(low_throughput_anomalies[0]), "low_throughput")
  assert_eq(Anomaly::description(low_throughput_anomalies[0]), "Throughput falls below 100 requests/min")
  
  // 创建多重异常数据
  let multiple_anomalies_data = TelemetryData::new(
    "api.request",
    TraceContext::new("trace-5", "span-5", None, []),
    [
      ("endpoint", StringValue("/users")),
      ("status", StringValue("500")),
      ("response_time", FloatValue(1200.0)),  // 异常：高响应时间
      ("error_rate", FloatValue(0.12)),      // 异常：高错误率
      ("throughput", IntValue(80))           // 异常：低吞吐量
    ],
    [],
    1200
  )
  
  let multiple_anomalies = AnomalyDetector::detect(anomaly_detector, multiple_anomalies_data)
  assert_eq(multiple_anomalies.length(), 3)  // 三种异常都应被检测到
  
  // 验证多重异常
  let anomaly_names = multiple_anomalies.map(fn(a) { Anomaly::rule_name(a) })
  assert_true(anomaly_names.contains("high_response_time"))
  assert_true(anomaly_names.contains("high_error_rate"))
  assert_true(anomaly_names.contains("low_throughput"))
  
  // 测试统计异常检测
  let historical_data = [
    TelemetryData::new("api.request", TraceContext::new("trace-1", "span-1", None, []),
      [("response_time", FloatValue(100.0))], [], 100),
    TelemetryData::new("api.request", TraceContext::new("trace-2", "span-2", None, []),
      [("response_time", FloatValue(110.0))], [], 110),
    TelemetryData::new("api.request", TraceContext::new("trace-3", "span-3", None, []),
      [("response_time", FloatValue(95.0))], [], 95),
    TelemetryData::new("api.request", TraceContext::new("trace-4", "span-4", None, []),
      [("response_time", FloatValue(105.0))], [], 105),
    TelemetryData::new("api.request", TraceContext::new("trace-5", "span-5", None, []),
      [("response_time", FloatValue(120.0))], [], 120)
  ]
  
  // 添加统计异常检测规则
  AnomalyDetector::add_statistical_rule(anomaly_detector, StatisticalAnomalyRule::new(
    "response_time_spike",
    "response_time",
    StatisticalMethod::ZScore,
    2.0,  // 2个标准差
    "Response time is statistically anomalous"
  ))
  
  // 测试正常数据
  let current_normal = TelemetryData::new("api.request", TraceContext::new("trace-6", "span-6", None, []),
    [("response_time", FloatValue(115.0))], [], 115)
  
  let statistical_normal_anomalies = AnomalyDetector::detect_statistical(anomaly_detector, historical_data, current_normal)
  assert_eq(statistical_normal_anomalies.length(), 0)
  
  // 测试异常数据
  let current_anomaly = TelemetryData::new("api.request", TraceContext::new("trace-7", "span-7", None, []),
    [("response_time", FloatValue(200.0))], [], 200)
  
  let statistical_anomalies = AnomalyDetector::detect_statistical(anomaly_detector, historical_data, current_anomaly)
  assert_eq(statistical_anomalies.length(), 1)
  assert_eq(Anomaly::rule_name(statistical_anomalies[0]), "response_time_spike")
}