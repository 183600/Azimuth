// Azimuth 高级遥测场景测试用例
// 专注于复杂遥测场景和边界情况测试

// 测试1: 实时流式数据处理
test "实时流式数据处理测试" {
  let stream_processor = StreamProcessor::new()
  let telemetry_sink = TelemetrySink::create("stream.processor")
  
  // 配置流式处理管道
  let pipeline = StreamProcessor::create_pipeline(stream_processor, [
    ("filter", "event.type == 'metric'"),
    ("transform", "convert_units()"),
    ("aggregate", "window_size=5s")
  ])
  
  // 模拟流式数据输入
  let stream_events = [
    MetricEvent::new("cpu.usage", 75.5, Time::now()),
    MetricEvent::new("memory.usage", 60.2, Time::now()),
    MetricEvent::new("network.throughput", 1024.0, Time::now()),
    MetricEvent::new("disk.io", 256.0, Time::now()),
    LogEvent::new("system.info", "System startup completed", Time::now())
  ]
  
  // 处理流式数据
  let processed_events = StreamProcessor::process_batch(pipeline, stream_events)
  
  // 验证处理结果
  assert_eq(processed_events.length(), 4) // 过滤掉日志事件
  assert_true(StreamProcessor::is_pipeline_healthy(pipeline))
  
  // 验证聚合结果
  let aggregated_metrics = StreamProcessor::get_aggregated_metrics(pipeline)
  assert_true(aggregated_metrics.contains_key("cpu.usage"))
  assert_true(aggregated_metrics.contains_key("memory.usage"))
}

// 测试2: 自适应采样策略
test "自适应采样策略测试" {
  let sampling_manager = AdaptiveSamplingManager::new()
  
  // 配置自适应采样策略
  let strategy = AdaptiveSamplingStrategy::new()
    .with_error_rate_threshold(0.05)
    .with_latency_threshold(100.0)
    .with_throughput_threshold(1000.0)
    .with_adjustment_factor(0.1)
  
  // 初始化采样率
  AdaptiveSamplingManager::set_strategy(sampling_manager, strategy)
  assert_eq(AdaptiveSamplingManager::get_current_rate(sampling_manager), 0.1)
  
  // 模拟高错误率场景
  let error_metrics = [
    ErrorMetric::new("database.timeout", 0.08),
    ErrorMetric::new("api.gateway.5xx", 0.12),
    ErrorMetric::new("cache.miss", 0.03)
  ]
  
  AdaptiveSamplingManager::update_error_metrics(sampling_manager, error_metrics)
  let adjusted_rate = AdaptiveSamplingManager::adjust_sampling_rate(sampling_manager)
  
  // 验证采样率调整
  assert_true(adjusted_rate > 0.1) // 错误率高，增加采样率
  assert_true(adjusted_rate <= 1.0)
  
  // 模拟低延迟高吞吐场景
  let performance_metrics = [
    LatencyMetric::new("api.response", 45.0),
    ThroughputMetric::new("requests.per.second", 1500.0)
  ]
  
  AdaptiveSamplingManager::update_performance_metrics(sampling_manager, performance_metrics)
  let optimized_rate = AdaptiveSamplingManager::optimize_sampling_rate(sampling_manager)
  
  // 验证优化后的采样率
  assert_true(optimized_rate < adjusted_rate) // 性能好，可以降低采样率
}

// 测试3: 异常检测和自动恢复
test "异常检测和自动恢复测试" {
  let anomaly_detector = AnomalyDetector::new()
  let recovery_manager = RecoveryManager::new()
  
  // 配置异常检测规则
  let detection_rules = [
    AnomalyRule::spike_detection("cpu.usage", 2.0, 5), // 2倍标准差，5个窗口
    AnomalyRule::trend_analysis("memory.leak", 0.1, 10), // 10%增长率，10个窗口
    AnomalyRule::pattern_matching("error.pattern", ".*timeout.*")
  ]
  
  AnomalyDetector::configure_rules(anomaly_detector, detection_rules)
  
  // 模拟正常时间序列数据
  let normal_metrics = TimeSeries::generate("cpu.usage", 50.0, 5.0, 100)
  let normal_result = AnomalyDetector::analyze(anomaly_detector, normal_metrics)
  assert_eq(normal_result.anomaly_detected, false)
  
  // 模拟异常时间序列数据
  let anomaly_metrics = TimeSeries::with_spike("cpu.usage", 50.0, 5.0, 95, 150.0, 5)
  let anomaly_result = AnomalyDetector::analyze(anomaly_detector, anomaly_metrics)
  assert_eq(anomaly_result.anomaly_detected, true)
  assert_eq(anomaly_result.anomaly_type, Spike)
  
  // 触发自动恢复
  if anomaly_result.anomaly_detected {
    let recovery_strategy = RecoveryManager::select_strategy(recovery_manager, anomaly_result)
    assert_eq(recovery_strategy.type, CircuitBreaker)
    
    let recovery_result = RecoveryManager::execute_recovery(recovery_manager, recovery_strategy)
    assert_eq(recovery_result.status, Success)
    assert_true(recovery_result.recovery_time_ms < 5000)
  }
}

// 测试4: 分布式链路追踪一致性
test "分布式链路追踪一致性测试" {
  let trace_consistency_validator = TraceConsistencyValidator::new()
  
  // 创建分布式追踪场景
  let trace_id = TraceId::generate()
  let root_span = Span::create_root(trace_id, "api.gateway.request")
  
  // 模拟微服务调用链
  let service_spans = [
    Span::create_child(trace_id, root_span.id, "auth.service.validate"),
    Span::create_child(trace_id, root_span.id, "user.service.profile"),
    Span::create_child(trace_id, root_span.id, "order.service.create"),
    Span::create_child(trace_id, root_span.id, "payment.service.process"),
    Span::create_child(trace_id, root_span.id, "notification.service.send")
  ]
  
  // 添加跨服务上下文传播
  for span in service_spans {
    Span::add_tag(span, "service.name", span.name.split(".")[0])
    Span::add_tag(span, "operation.name", span.name.split(".")[1])
    Span::add_tag(span, "trace.parent", root_span.id)
  }
  
  // 验证追踪一致性
  let consistency_result = TraceConsistencyValidator::validate(trace_consistency_validator, trace_id, [root_span] + service_spans)
  
  assert_eq(consistency_result.is_consistent, true)
  assert_eq(consistency_result.total_spans, 6)
  assert_eq(consistency_result.max_depth, 2)
  assert_eq(consistency_result.gaps.length(), 0)
  
  // 验证时间顺序
  let time_ordered_spans = TraceConsistencyValidator::order_by_time(consistency_result.spans)
  assert_true(time_ordered_spans[0].start_time <= time_ordered_spans[1].start_time)
}

// 测试5: 多维度指标聚合
test "多维度指标聚合测试" {
  let aggregator = MultiDimensionalAggregator::new()
  
  // 创建多维度指标数据
  let metrics = [
    Metric::with_dimensions("response.time", 120.0, [
      ("service", "api.gateway"),
      ("endpoint", "/users"),
      ("method", "GET"),
      ("status.code", "200")
    ]),
    Metric::with_dimensions("response.time", 250.0, [
      ("service", "api.gateway"),
      ("endpoint", "/users"),
      ("method", "POST"),
      ("status.code", "201")
    ]),
    Metric::with_dimensions("response.time", 5000.0, [
      ("service", "database"),
      ("operation", "query"),
      ("table", "users"),
      ("status", "timeout")
    ]),
    Metric::with_dimensions("response.time", 80.0, [
      ("service", "cache"),
      ("operation", "get"),
      ("key", "user:123"),
      ("hit", "true")
    ])
  ]
  
  // 配置聚合规则
  let aggregation_rules = [
    AggregationRule::average("service"),
    AggregationRule::percentile("endpoint", [50.0, 95.0, 99.0]),
    AggregationRule::rate("status.code", "200"),
    AggregationRule::group_by("service", "method")
  ]
  
  MultiDimensionalAggregator::configure_rules(aggregator, aggregation_rules)
  
  // 执行聚合
  let aggregation_result = MultiDimensionalAggregator::aggregate(aggregator, metrics)
  
  // 验证聚合结果
  assert_true(aggregation_result.contains_key("service.average"))
  assert_true(aggregation_result.contains_key("endpoint.percentile"))
  
  let api_gateway_avg = aggregation_result.get("service.average").get("api.gateway")
  assert_true(api_gateway_avg > 100.0 && api_gateway_avg < 300.0)
  
  let get_percentiles = aggregation_result.get("endpoint.percentile").get("/users")
  assert_true(get_percentiles.contains_key("p50"))
  assert_true(get_percentiles.contains_key("p95"))
  assert_true(get_percentiles.contains_key("p99"))
}

// 测试6: 智能告警和降噪
test "智能告警和降噪测试" {
  let alerting_system = IntelligentAlertingSystem::new()
  
  // 配置告警规则
  let alert_rules = [
    AlertRule::threshold("error.rate", 0.05, GreaterThan, 300),
    AlertRule::anomaly("latency.spike", 2.0, 5),
    AlertRule::pattern("log.error", ".*critical.*", 60),
    AlertRule::composite("service.degradation", [
      ("error.rate", ">", 0.03),
      ("latency.p95", ">", 1000.0)
    ], "AND")
  ]
  
  IntelligentAlertingSystem::configure_rules(alerting_system, alert_rules)
  
  // 配置降噪策略
  let noise_reduction_strategies = [
    NoiseReductionStrategy::frequency_limiting("same.alert", 300),
    NoiseReductionStrategy::correlation_grouping("related.services"),
    NoiseReductionStrategy::severity_escalation("gradual.increase"),
    NoiseReductionStrategy::maintenance_window("scheduled.downtime")
  ]
  
  IntelligentAlertingSystem::configure_noise_reduction(alerting_system, noise_reduction_strategies)
  
  // 模拟告警触发
  let alert_events = [
    AlertEvent::new("error.rate", 0.08, Time::now()),
    AlertEvent::new("error.rate", 0.09, Time::now().add_seconds(60)),
    AlertEvent::new("error.rate", 0.07, Time::now().add_seconds(120)),
    AlertEvent::new("latency.spike", 3.5, Time::now().add_seconds(180))
  ]
  
  // 处理告警事件
  let processed_alerts = IntelligentAlertingSystem::process_events(alerting_system, alert_events)
  
  // 验证降噪效果
  assert_true(processed_alerts.length() < alert_events.length()) // 应该有降噪效果
  
  let deduplicated_alerts = IntelligentAlertingSystem::get_deduplicated_alerts(alerting_system)
  assert_eq(deduplicated_alerts.length(), 1) // error.rate应该被去重
  
  let escalated_alerts = IntelligentAlertingSystem::get_escalated_alerts(alerting_system)
  assert_true(escalated_alerts.length() > 0) // 应该有升级的告警
}

// 测试7: 遥测数据生命周期管理
test "遥测数据生命周期管理测试" {
  let lifecycle_manager = TelemetryDataLifecycleManager::new()
  
  // 配置生命周期策略
  let lifecycle_policies = [
    LifecyclePolicy::retention("metrics", 30, "days"),
    LifecyclePolicy::retention("traces", 7, "days"),
    LifecyclePolicy::retention("logs", 90, "days"),
    LifecyclePolicy::compression("old.data", 30, "days"),
    LifecyclePolicy::archival("historical.data", 365, "days"),
    LifecyclePolicy::deletion("expired.data", 400, "days")
  ]
  
  TelemetryDataLifecycleManager::configure_policies(lifecycle_manager, lifecycle_policies)
  
  // 创建不同年龄的测试数据
  let current_time = Time::now()
  let test_data = [
    TelemetryData::new("metrics", current_time.add_days(-5)), // 5天前
    TelemetryData::new("traces", current_time.add_days(-10)), // 10天前
    TelemetryData::new("logs", current_time.add_days(-30)), // 30天前
    TelemetryData::new("metrics", current_time.add_days(-60)), // 60天前
    TelemetryData::new("traces", current_time.add_days(-400)) // 400天前
  ]
  
  // 执行生命周期管理
  let lifecycle_result = TelemetryDataLifecycleManager::process(lifecycle_manager, test_data)
  
  // 验证处理结果
  assert_eq(lifecycle_result.to_keep.length(), 3) // 5天前metrics, 10天前traces, 30天前logs
  assert_eq(lifecycle_result.to_compress.length(), 1) // 60天前metrics
  assert_eq(lifecycle_result.to_archive.length(), 0) // 没有365天的数据
  assert_eq(lifecycle_result.to_delete.length(), 1) // 400天前traces
  
  // 验证存储空间优化
  let space_saved = TelemetryDataLifecycleManager::calculate_space_saved(lifecycle_manager)
  assert_true(space_saved.compression_ratio > 0.5)
  assert_true(space_saved.deletion_freed_mb > 0)
}

// 测试8: 跨云环境遥测同步
test "跨云环境遥测同步测试" {
  let cross_cloud_sync = CrossCloudTelemetrySync::new()
  
  // 配置云环境
  let cloud_environments = [
    CloudEnvironment::new("aws.prod", "us-east-1", "otel-collector.aws.example.com"),
    CloudEnvironment::new("gcp.prod", "us-central1", "otel-collector.gcp.example.com"),
    CloudEnvironment::new("azure.prod", "eastus", "otel-collector.azure.example.com")
  ]
  
  CrossCloudTelemetrySync::configure_environments(cross_cloud_sync, cloud_environments)
  
  // 配置同步策略
  let sync_strategies = [
    SyncStrategy::real_time("critical.alerts"),
    SyncStrategy::batch("metrics", 60),
    SyncStrategy::selective("traces", "error.rate > 0.01"),
    SyncStrategy::compressed("logs", "gzip")
  ]
  
  CrossCloudTelemetrySync::configure_strategies(cross_cloud_sync, sync_strategies)
  
  // 模拟跨云数据
  let aws_data = TelemetryData::with_source("aws.prod", [
    Metric::new("cpu.usage", 65.5),
    Trace::new("api.request", "trace-aws-123"),
    Log::new("system.startup", "AWS service started")
  ])
  
  let gcp_data = TelemetryData::with_source("gcp.prod", [
    Metric::new("memory.usage", 72.3),
    Trace::new("api.request", "trace-gcp-456"),
    Log::new("system.startup", "GCP service started")
  ])
  
  // 执行同步
  let sync_result = CrossCloudTelemetrySync::sync(cross_cloud_sync, [aws_data, gcp_data])
  
  // 验证同步结果
  assert_eq(sync_result.successful_syncs, 2)
  assert_eq(sync_result.failed_syncs, 0)
  assert_true(sync_result.total_bytes_transferred > 0)
  
  // 验证数据一致性
  let consistency_check = CrossCloudTelemetrySync::verify_consistency(cross_cloud_sync)
  assert_eq(consistency_check.is_consistent, true)
  assert_eq(consistency_check.inconsistencies.length(), 0)
}

// 测试9: 遥测数据质量评估
test "遥测数据质量评估测试" {
  let quality_assessor = TelemetryDataQualityAssessor::new()
  
  // 配置质量评估规则
  let quality_rules = [
    QualityRule::completeness("required.fields", ["trace.id", "timestamp", "service.name"]),
    QualityRule::accuracy("numeric.ranges", [
      ("cpu.usage", 0.0, 100.0),
      ("memory.usage", 0.0, 100.0),
      ("response.time", 0.0, 300000.0)
    ]),
    QualityRule::consistency("timestamp.order", "ascending"),
    QualityRule::validity("enum.values", [
      ("span.kind", ["server", "client", "producer", "consumer"]),
      ("status.code", ["ok", "error"])
    ])
  ]
  
  TelemetryDataQualityAssessor::configure_rules(quality_assessor, quality_rules)
  
  // 创建测试数据集
  let test_datasets = [
    // 高质量数据集
    TelemetryDataset::new("high.quality", [
      Metric::with_attributes("cpu.usage", 65.5, [
        ("trace.id", "trace-123"),
        ("timestamp", "2023-01-01T12:00:00Z"),
        ("service.name", "api.service")
      ])
    ]),
    
    // 低质量数据集 - 缺少必需字段
    TelemetryDataset::new("low.quality", [
      Metric::with_attributes("cpu.usage", 65.5, [
        ("timestamp", "2023-01-01T12:00:00Z")
        // 缺少trace.id和service.name
      ])
    ]),
    
    // 异常数据集 - 数值超出范围
    TelemetryDataset::new("anomalous.data", [
      Metric::with_attributes("cpu.usage", 150.0, [ // 超出0-100范围
        ("trace.id", "trace-456"),
        ("timestamp", "2023-01-01T12:00:00Z"),
        ("service.name", "api.service")
      ])
    ])
  ]
  
  // 执行质量评估
  let quality_results = TelemetryDataQualityAssessor::assess(quality_assessor, test_datasets)
  
  // 验证评估结果
  assert_eq(quality_results.length(), 3)
  
  let high_quality_score = quality_results[0].overall_score
  assert_true(high_quality_score > 0.9) // 高质量数据集应该得分很高
  
  let low_quality_score = quality_results[1].overall_score
  assert_true(low_quality_score < 0.7) // 低质量数据集应该得分较低
  
  let anomalous_score = quality_results[2].overall_score
  assert_true(anomalous_score < 0.8) // 异常数据集应该得分中等偏低
  
  // 验证具体质量问题
  let low_quality_issues = quality_results[1].issues
  assert_true(low_quality_issues.contains(CompletenessIssue))
  
  let anomalous_issues = quality_results[2].issues
  assert_true(anomalous_issues.contains(AccuracyIssue))
}

// 测试10: 遥测系统自监控
test "遥测系统自监控测试" {
  let self_monitoring = TelemetrySystemSelfMonitoring::new()
  
  // 配置自监控指标
  let monitoring_metrics = [
    SelfMonitoringMetric::component_health("otel.collector"),
    SelfMonitoringMetric::throughput("data.processing"),
    SelfMonitoringMetric::latency("alert.generation"),
    SelfMonitoringMetric::resource_usage("memory", "cpu", "disk"),
    SelfMonitoringMetric::error_rate("data.serialization", "network.transmission")
  ]
  
  TelemetrySystemSelfMonitoring::configure_metrics(self_monitoring, monitoring_metrics)
  
  // 模拟系统运行状态
  let system_status = SystemStatus::new()
  SystemStatus::set_component_health(system_status, "otel.collector", Healthy)
  SystemStatus::set_component_health(system_status, "data.processor", Healthy)
  SystemStatus::set_component_health(system_status, "alert.manager", Degraded)
  
  SystemStatus::set_throughput(system_status, "data.processing", 5000.0)
  SystemStatus::set_latency(system_status, "alert.generation", 150.0)
  SystemStatus::set_resource_usage(system_status, "memory", 75.5)
  SystemStatus::set_resource_usage(system_status, "cpu", 60.2)
  SystemStatus::set_error_rate(system_status, "data.serialization", 0.001)
  
  // 执行自监控检查
  let monitoring_result = TelemetrySystemSelfMonitoring::check(self_monitoring, system_status)
  
  // 验证监控结果
  assert_eq(monitoring_result.overall_health, Degraded) // 有一个组件降级
  assert_eq(monitoring_result.healthy_components, 2)
  assert_eq(monitoring_result.degraded_components, 1)
  assert_eq(monitoring_result.failed_components, 0)
  
  // 验证性能指标
  assert_true(monitoring_result.throughput_metrics.get("data.processing") > 4000.0)
  assert_true(monitoring_result.latency_metrics.get("alert.generation") < 200.0)
  assert_true(monitoring_result.resource_usage.get("memory") < 80.0)
  assert_true(monitoring_result.error_rates.get("data.serialization") < 0.01)
  
  // 生成健康报告
  let health_report = TelemetrySystemSelfMonitoring::generate_health_report(self_monitoring, monitoring_result)
  assert_true(health_report.contains("Overall Health: Degraded"))
  assert_true(health_report.contains("Healthy Components: 2"))
  assert_true(health_report.contains("Degraded Components: 1"))
}