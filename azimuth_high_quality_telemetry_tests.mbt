// Azimuth 高质量遥测系统测试用例
// 专注于遥测系统的高级功能和复杂场景测试

// 测试1: 自适应采样策略
test "自适应采样策略测试" {
  // 创建自适应采样管理器
  let sampling_manager = AdaptiveSamplingManager::new()
  
  // 设置采样策略
  let error_sampling_strategy = ErrorBasedSamplingStrategy::new(0.1, 0.8)
  let latency_sampling_strategy = LatencyBasedSamplingStrategy::new(0.05, 100.0)
  let volume_sampling_strategy = VolumeBasedSamplingStrategy::new(1000, 0.2)
  
  AdaptiveSamplingManager::add_strategy(sampling_manager, "error_based", error_sampling_strategy)
  AdaptiveSamplingManager::add_strategy(sampling_manager, "latency_based", latency_sampling_strategy)
  AdaptiveSamplingManager::add_strategy(sampling_manager, "volume_based", volume_sampling_strategy)
  
  // 创建测试追踪上下文
  let normal_trace = TraceContext::new("trace-normal-001", "span-001", 0.050, false)
  let error_trace = TraceContext::new("trace-error-002", "span-002", 0.200, true)
  let high_latency_trace = TraceContext::new("trace-latency-003", "span-003", 0.150, false)
  
  // 测试采样决策
  let normal_decision = AdaptiveSamplingManager::should_sample(sampling_manager, normal_trace)
  let error_decision = AdaptiveSamplingManager::should_sample(sampling_manager, error_trace)
  let latency_decision = AdaptiveSamplingManager::should_sample(sampling_manager, high_latency_trace)
  
  // 验证采样决策
  assert_true(error_decision.sampled)  // 错误追踪应该被采样
  assert_true(latency_decision.sampled)  // 高延迟追踪应该被采样
  
  // 测试采样率动态调整
  let volume_metrics = VolumeMetrics::new(1500, 10000)  // 超过阈值
  AdaptiveSamplingManager::update_metrics(sampling_manager, volume_metrics)
  
  let adjusted_normal_decision = AdaptiveSamplingManager::should_sample(sampling_manager, normal_trace)
  assert_true(adjusted_normal_decision.sampling_rate < normal_decision.sampling_rate)
}

// 测试2: 实时异常检测和告警
test "实时异常检测和告警测试" {
  // 创建异常检测引擎
  let anomaly_detector = AnomalyDetectionEngine::new()
  
  // 配置检测规则
  let error_rate_rule = ErrorRateAnomalyRule::new(0.05, 5.0)  // 5分钟内错误率超过5%
  let latency_spike_rule = LatencySpikeAnomalyRule::new(2.0, 10)  // P99延迟超过2倍基线
  let throughput_drop_rule = ThroughputDropRule::new(0.5, 3.0)  // 吞吐量下降50%超过3分钟
  
  AnomalyDetectionEngine::add_rule(anomaly_detector, "error_rate", error_rate_rule)
  AnomalyDetectionEngine::add_rule(anomaly_detector, "latency_spike", latency_spike_rule)
  AnomalyDetectionEngine::add_rule(anomaly_detector, "throughput_drop", throughput_drop_rule)
  
  // 模拟正常指标数据
  let normal_metrics = TelemetryMetrics::new()
  TelemetryMetrics::add_counter(normal_metrics, "requests.total", 1000.0)
  TelemetryMetrics::add_counter(normal_metrics, "errors.total", 20.0)  // 2%错误率
  TelemetryMetrics::add_histogram(normal_metrics, "response.time", [0.050, 0.075, 0.100])
  
  let normal_result = AnomalyDetectionEngine::analyze(anomaly_detector, normal_metrics)
  assert_eq(normal_result.anomalies.length(), 0)
  
  // 模拟异常指标数据
  let anomaly_metrics = TelemetryMetrics::new()
  TelemetryMetrics::add_counter(anomaly_metrics, "requests.total", 800.0)
  TelemetryMetrics::add_counter(anomaly_metrics, "errors.total", 120.0)  // 15%错误率
  TelemetryMetrics::add_histogram(anomaly_metrics, "response.time", [0.200, 0.350, 0.500])
  
  let anomaly_result = AnomalyDetectionEngine::analyze(anomaly_detector, anomaly_metrics)
  assert_true(anomaly_result.anomalies.length() >= 2)
  
  // 验证告警生成
  let alerts = AnomalyDetectionEngine::generate_alerts(anomaly_detector, anomaly_result)
  assert_true(alerts.length() >= 2)
  
  // 验证告警严重程度
  let critical_alerts = alerts.filter(fn(alert) { alert.severity == Critical })
  assert_true(critical_alerts.length() >= 1)
}

// 测试3: 遥测数据智能聚合
test "遥测数据智能聚合测试" {
  // 创建数据聚合器
  let aggregator = TelemetryDataAggregator::new()
  
  // 配置聚合策略
  let time_window_strategy = TimeWindowStrategy::new(60000, SlidingWindow)  // 1分钟滑动窗口
  let service_dimension_strategy = ServiceDimensionStrategy::new(["service.name", "endpoint"])
  let percentile_aggregation_strategy = PercentileAggregationStrategy::new([50.0, 90.0, 95.0, 99.0])
  
  TelemetryDataAggregator::add_strategy(aggregator, time_window_strategy)
  TelemetryDataAggregator::add_strategy(aggregator, service_dimension_strategy)
  TelemetryDataAggregator::add_strategy(aggregator, percentile_aggregation_strategy)
  
  // 添加测试数据点
  let data_points = [
    DataPoint::new("api.request.duration", 0.050, ["service:user-api", "endpoint:/users"]),
    DataPoint::new("api.request.duration", 0.075, ["service:user-api", "endpoint:/users"]),
    DataPoint::new("api.request.duration", 0.100, ["service:user-api", "endpoint:/users"]),
    DataPoint::new("api.request.duration", 0.025, ["service:payment-api", "endpoint:/payments"]),
    DataPoint::new("api.request.duration", 0.150, ["service:payment-api", "endpoint:/payments"]),
    DataPoint::new("api.request.duration", 0.200, ["service:payment-api", "endpoint:/payments"])
  ]
  
  for point in data_points {
    TelemetryDataAggregator::add_data_point(aggregator, point)
  }
  
  // 执行聚合
  let aggregation_result = TelemetryDataAggregator::aggregate(aggregator)
  
  // 验证聚合结果
  let user_api_metrics = aggregation_result.get_metrics(["service:user-api", "endpoint:/users"])
  assert_eq(user_api_metrics.count, 3)
  assert_eq(user_api_metrics.min, 0.050)
  assert_eq(user_api_metrics.max, 0.100)
  
  let payment_api_metrics = aggregation_result.get_metrics(["service:payment-api", "endpoint:/payments"])
  assert_eq(payment_api_metrics.count, 3)
  assert_eq(payment_api_metrics.min, 0.025)
  assert_eq(payment_api_metrics.max, 0.200)
  
  // 验证百分位数计算
  let user_api_p95 = user_api_metrics.percentiles.get(95.0)
  assert_true(user_api_p95 >= 0.090 and user_api_p95 <= 0.100)
}

// 测试4: 高性能数据缓冲和批处理
test "高性能数据缓冲和批处理测试" {
  // 创建高性能缓冲管理器
  let buffer_manager = HighPerformanceBufferManager::new()
  
  // 配置缓冲策略
  let memory_buffer_strategy = MemoryBufferStrategy::new(100 * 1024 * 1024)  // 100MB内存缓冲
  let disk_buffer_strategy = DiskBufferStrategy::new("/tmp/telemetry-buffer", 1024 * 1024 * 1024)  // 1GB磁盘缓冲
  let batch_strategy = AdaptiveBatchStrategy::new(1000, 5000, 5000)  // 1K-5K动态批处理
  
  HighPerformanceBufferManager::configure_memory_buffer(buffer_manager, memory_buffer_strategy)
  HighPerformanceBufferManager::configure_disk_buffer(buffer_manager, disk_buffer_strategy)
  HighPerformanceBufferManager::configure_batch_strategy(buffer_manager, batch_strategy)
  
  // 模拟高并发数据写入
  let concurrent_writers = 10
  let writes_per_writer = 1000
  let write_results = []
  
  for i in 0..concurrent_writers {
    let writer_id = "writer-" + i.to_string()
    let writer_results = []
    
    for j in 0..writes_per_writer {
      let data_point = TelemetryDataPoint::new(
        "high.throughput.metric",
        (i * writes_per_writer + j).to_float(),
        ["writer:" + writer_id, "batch:" + (j / 100).to_string()]
      )
      
      let write_result = HighPerformanceBufferManager::write(buffer_manager, data_point)
      writer_results = writer_results.push(write_result)
    }
    
    write_results = write_results.push(writer_results)
  }
  
  // 验证所有写入成功
  let total_writes = concurrent_writers * writes_per_writer
  let successful_writes = write_results.flatten().filter(fn(result) { result.success }).length()
  assert_eq(successful_writes, total_writes)
  
  // 测试批处理触发
  let batch_results = HighPerformanceBufferManager::flush_batches(buffer_manager)
  assert_true(batch_results.length() > 0)
  
  // 验证批处理性能
  let total_processed = batch_results.reduce(fn(acc, batch) { acc + batch.processed_count }, 0)
  assert_eq(total_processed, total_writes)
  
  // 验证内存使用效率
  let memory_stats = HighPerformanceBufferManager::get_memory_stats(buffer_manager)
  assert_true(memory_stats.utilization < 0.8)  // 内存使用率应该低于80%
}

// 测试5: 分布式追踪深度链路分析
test "分布式追踪深度链路分析测试" {
  // 创建链路分析器
  let trace_analyzer = DistributedTraceAnalyzer::new()
  
  // 创建复杂的分布式追踪链路
  let root_span = Span::new("api.gateway.request", Server, SpanContext::new("trace-001", "span-001", true, ""))
  Span::set_attribute(root_span, "http.method", StringValue("POST"))
  Span::set_attribute(root_span, "http.url", StringValue("/api/v1/process"))
  
  // 第一层服务调用
  let auth_span = Span::new_child("auth.service.validate", root_span)
  Span::set_attribute(auth_span, "service.name", StringValue("auth-service"))
  Span::set_duration(auth_span, 0.025)
  
  let business_span = Span::new_child("business.logic.process", root_span)
  Span::set_attribute(business_span, "service.name", StringValue("business-service"))
  
  // 第二层服务调用
  let db_span = Span::new_child("database.query", business_span)
  Span::set_attribute(db_span, "db.system", StringValue("postgresql"))
  Span::set_attribute(db_span, "db.statement", StringValue("SELECT * FROM orders"))
  Span::set_duration(db_span, 0.150)
  
  let cache_span = Span::new_child("cache.get", business_span)
  Span::set_attribute(cache_span, "cache.system", StringValue("redis"))
  Span::set_duration(cache_span, 0.005)
  
  let external_span = Span::new_child("external.api.call", business_span)
  Span::set_attribute(external_span, "http.url", StringValue("https://external.service.com/api"))
  Span::set_duration(external_span, 0.200)
  
  // 添加所有span到分析器
  let all_spans = [root_span, auth_span, business_span, db_span, cache_span, external_span]
  for span in all_spans {
    DistributedTraceAnalyzer::add_span(trace_analyzer, span)
  }
  
  // 执行链路分析
  let analysis_result = DistributedTraceAnalyzer::analyze(trace_analyzer)
  
  // 验证链路深度和广度
  assert_eq(analysis_result.trace_depth, 3)  // 三层调用深度
  assert_eq(analysis_result.service_count, 4)  // 4个不同服务
  assert_eq(analysis_result.total_spans, 6)
  
  // 验证关键路径分析
  let critical_path = analysis_result.critical_path
  assert_eq(critical_path.length(), 3)
  assert_eq(critical_path[2].name, "external.api.call")  // 最耗时的操作
  
  // 验证性能瓶颈识别
  let bottlenecks = analysis_result.performance_bottlenecks
  assert_true(bottlenecks.length() >= 2)
  assert_true(bottlenecks.any(fn(b) { b.span_name == "database.query" }))
  assert_true(bottlenecks.any(fn(b) { b.span_name == "external.api.call" }))
  
  // 验证服务依赖分析
  let dependencies = analysis_result.service_dependencies
  assert_true(dependencies.contains(("business-service", "postgresql")))
  assert_true(dependencies.contains(("business-service", "redis")))
  assert_true(dependencies.contains(("business-service", "external.service")))
}

// 测试6: 遥测数据质量验证
test "遥测数据质量验证测试" {
  // 创建数据质量验证器
  let quality_validator = TelemetryDataQualityValidator::new()
  
  // 配置质量规则
  let completeness_rule = CompletenessRule::new(["trace_id", "span_id", "timestamp"], 0.95)
  let consistency_rule = ConsistencyRule::new("timestamp", "duration", fn(timestamp, duration) { 
    timestamp > 0 and duration >= 0 
  })
  let accuracy_rule = AccuracyRule::new("response_time", 0.0, 300000.0)  // 0-5分钟
  let uniqueness_rule = UniquenessRule::new("span_id", 1.0)  // span_id必须唯一
  
  TelemetryDataQualityValidator::add_rule(quality_validator, completeness_rule)
  TelemetryDataQualityValidator::add_rule(quality_validator, consistency_rule)
  TelemetryDataQualityValidator::add_rule(quality_validator, accuracy_rule)
  TelemetryDataQualityValidator::add_rule(quality_validator, uniqueness_rule)
  
  // 创建高质量测试数据
  let quality_data_points = [
    QualityDataPoint::new("span-001", "trace-001", 1640995200, 0.050, ["service:api"]),
    QualityDataPoint::new("span-002", "trace-001", 1640995250, 0.025, ["service:auth"]),
    QualityDataPoint::new("span-003", "trace-002", 1640995300, 0.150, ["service:db"])
  ]
  
  let quality_result = TelemetryDataQualityValidator::validate(quality_validator, quality_data_points)
  assert_true(quality_result.overall_score >= 0.9)
  assert_eq(quality_result.violations.length(), 0)
  
  // 创建低质量测试数据
  let poor_quality_data_points = [
    QualityDataPoint::new("", "trace-003", 1640995350, 0.075, ["service:api"]),  // 缺少span_id
    QualityDataPoint::new("span-004", "", 1640995400, -0.010, ["service:auth"]),  // 缺少trace_id和负duration
    QualityDataPoint::new("span-003", "trace-004", 1640995450, 500000.0, ["service:db"]),  // 超时响应
    QualityDataPoint::new("span-005", "trace-005", 1640995500, 0.100, ["service:cache"])
  ]
  
  let poor_quality_result = TelemetryDataQualityValidator::validate(quality_validator, poor_quality_data_points)
  assert_true(poor_quality_result.overall_score < 0.7)
  assert_true(poor_quality_result.violations.length() >= 3)
  
  // 验证质量报告生成
  let quality_report = TelemetryDataQualityValidator::generate_report(quality_validator, poor_quality_result)
  assert_true(quality_report.recommendations.length() >= 2)
  assert_true(quality_report.affected_data_points >= 3)
}

// 测试7: 动态资源管理和自适应配置
test "动态资源管理和自适应配置测试" {
  // 创建动态资源管理器
  let resource_manager = DynamicResourceManager::new()
  
  // 配置资源监控
  let cpu_monitor = ResourceMonitor::new("cpu", 0.8, 0.9)  // 80%警告，90%临界
  let memory_monitor = ResourceMonitor::new("memory", 0.75, 0.85)
  let disk_monitor = ResourceMonitor::new("disk", 0.7, 0.8)
  
  DynamicResourceManager::add_monitor(resource_manager, cpu_monitor)
  DynamicResourceManager::add_monitor(resource_manager, memory_monitor)
  DynamicResourceManager::add_monitor(resource_manager, disk_monitor)
  
  // 配置自适应策略
  let sampling_adaptation = SamplingAdaptationStrategy::new(
    fn(cpu_usage, memory_usage) { 
      if cpu_usage > 0.8 or memory_usage > 0.8 { 0.05 } else { 0.1 } 
    }
  )
  
  let buffer_adaptation = BufferAdaptationStrategy::new(
    fn(available_memory) { 
      if available_memory < 100 * 1024 * 1024 { 500 } else { 2000 } 
    }
  )
  
  DynamicResourceManager::add_adaptation_strategy(resource_manager, "sampling", sampling_adaptation)
  DynamicResourceManager::add_adaptation_strategy(resource_manager, "buffer", buffer_adaptation)
  
  // 模拟资源使用情况变化
  let normal_resources = ResourceUsage::new(0.4, 0.5, 0.3)  // 正常负载
  let high_resources = ResourceUsage::new(0.85, 0.9, 0.6)   // 高负载
  
  // 测试正常负载下的配置
  let normal_config = DynamicResourceManager::adapt_configuration(resource_manager, normal_resources)
  assert_eq(normal_config.sampling_rate, 0.1)
  assert_eq(normal_config.buffer_size, 2000)
  
  // 测试高负载下的配置
  let high_config = DynamicResourceManager::adapt_configuration(resource_manager, high_resources)
  assert_eq(high_config.sampling_rate, 0.05)
  assert_eq(high_config.buffer_size, 500)
  
  // 验证配置应用
  let config_applier = ConfigurationApplier::new()
  ConfigurationApplier::apply_sampling_config(config_applier, high_config.sampling_rate)
  ConfigurationApplier::apply_buffer_config(config_applier, high_config.buffer_size)
  
  let applied_config = ConfigurationApplier::get_current_config(config_applier)
  assert_eq(applied_config.sampling_rate, 0.05)
  assert_eq(applied_config.buffer_size, 500)
  
  // 测试资源预警
  let alerts = DynamicResourceManager::check_resource_alerts(resource_manager, high_resources)
  assert_true(alerts.length() >= 2)
  assert_true(alerts.any(fn(alert) { alert.resource == "cpu" and alert.level == Critical }))
  assert_true(alerts.any(fn(alert) { alert.resource == "memory" and alert.level == Critical }))
}

// 测试8: 遥测系统弹性伸缩和容错
test "遥测系统弹性伸缩和容错测试" {
  // 创建弹性伸缩管理器
  let scalability_manager = TelemetryScalabilityManager::new()
  
  // 配置伸缩规则
  let load_based_scaling = LoadBasedScalingRule::new(
    1000,    // 每秒请求数阈值
    0.7,     // CPU使用率阈值
    2,       // 最小实例数
    10,      // 最大实例数
    30       // 缩放 cooldown 秒数
  )
  
  let latency_based_scaling = LatencyBasedScalingRule::new(
    0.100,   // P99延迟阈值
    2,       // 最小实例数
    8,       // 最大实例数
    60       // 缩放 cooldown 秒数
  )
  
  TelemetryScalabilityManager::add_scaling_rule(scalability_manager, "load_based", load_based_scaling)
  TelemetryScalabilityManager::add_scaling_rule(scalability_manager, "latency_based", latency_based_scaling)
  
  // 配置容错策略
  let circuit_breaker = CircuitBreakerStrategy::new(5, 60, 30)  // 5次失败，60秒超时，30秒恢复
  let retry_policy = ExponentialBackoffRetryStrategy::new(3, 1000, 2.0)  // 3次重试，1秒基础延迟，2倍递增
  let fallback_strategy = FallbackStrategy::new("backup.telemetry.collector")
  
  TelemetryScalabilityManager::add_fault_tolerance_strategy(scalability_manager, circuit_breaker)
  TelemetryScalabilityManager::add_fault_tolerance_strategy(scalability_manager, retry_policy)
  TelemetryScalabilityManager::add_fault_tolerance_strategy(scalability_manager, fallback_strategy)
  
  // 模拟负载变化
  let low_load_metrics = SystemMetrics::new(500, 0.4, 0.050)  // 低负载
  let high_load_metrics = SystemMetrics::new(2000, 0.8, 0.150)  // 高负载
  let extreme_load_metrics = SystemMetrics::new(5000, 0.95, 0.300)  // 极高负载
  
  // 测试低负载下的缩放决策
  let low_scaling_decision = TelemetryScalabilityManager::evaluate_scaling(scalability_manager, low_load_metrics)
  assert_eq(low_scaling_decision.action, ScaleDown)
  assert_true(low_scaling_decision.target_instances <= 4)
  
  // 测试高负载下的缩放决策
  let high_scaling_decision = TelemetryScalabilityManager::evaluate_scaling(scalability_manager, high_load_metrics)
  assert_eq(high_scaling_decision.action, ScaleUp)
  assert_true(high_scaling_decision.target_instances >= 4)
  
  // 测试极高负载下的缩放决策
  let extreme_scaling_decision = TelemetryScalabilityManager::evaluate_scaling(scalability_manager, extreme_load_metrics)
  assert_eq(extreme_scaling_decision.action, ScaleUp)
  assert_eq(extreme_scaling_decision.target_instances, 10)  // 达到最大实例数
  
  // 测试容错机制
  let fault_scenario = FaultScenario::new("collector.failure", 0.9)  // 90%失败率
  let fault_response = TelemetryScalabilityManager::handle_fault(scalability_manager, fault_scenario)
  
  assert_true(fault_response.circuit_breaker_opened)
  assert_true(fault_response.retries_attempted >= 1)
  assert_true(fault_response.fallback_used)
  
  // 测试恢复机制
  let recovery_scenario = FaultScenario::new("collector.recovery", 0.1)  // 10%失败率
  let recovery_response = TelemetryScalabilityManager::handle_fault(scalability_manager, recovery_scenario)
  
  assert_true(recovery_response.circuit_breaker_closed)
  assert_eq(recovery_response.retries_attempted, 0)  // 无需重试
}

// 测试9: 跨服务遥测数据一致性
test "跨服务遥测数据一致性测试" {
  // 创建一致性管理器
  let consistency_manager = CrossServiceConsistencyManager::new()
  
  // 配置服务拓扑
  let service_topology = ServiceTopology::new()
  ServiceTopology::add_service(service_topology, "api-gateway", ["auth-service", "user-service", "order-service"])
  ServiceTopology::add_service(service_topology, "auth-service", ["user-database"])
  ServiceTopology::add_service(service_topology, "user-service", ["user-database", "notification-service"])
  ServiceTopology::add_service(service_topology, "order-service", ["order-database", "payment-service"])
  
  CrossServiceConsistencyManager::set_topology(consistency_manager, service_topology)
  
  // 配置一致性规则
  let trace_id_consistency = TraceIdConsistencyRule::new()
  let timestamp_consistency = TimestampConsistencyRule::new(5000)  // 5秒误差范围
  let baggage_consistency = BaggageConsistencyRule::new(["user.id", "request.id", "tenant.id"])
  
  CrossServiceConsistencyManager::add_consistency_rule(consistency_manager, trace_id_consistency)
  CrossServiceConsistencyManager::add_consistency_rule(consistency_manager, timestamp_consistency)
  CrossServiceConsistencyManager::add_consistency_rule(consistency_manager, baggage_consistency)
  
  // 创建一致的多服务追踪数据
  let consistent_trace = MultiServiceTrace::new("consistent-trace-001")
  MultiServiceTrace::add_span(consistent_trace, "api-gateway", SpanData::new("span-001", 1640995200, 1640995250))
  MultiServiceTrace::add_span(consistent_trace, "auth-service", SpanData::new("span-002", 1640995250, 1640995270))
  MultiServiceTrace::add_span(consistent_trace, "user-service", SpanData::new("span-003", 1640995270, 1640995300))
  MultiServiceTrace::add_baggage(consistent_trace, "user.id", "user-12345")
  MultiServiceTrace::add_baggage(consistent_trace, "request.id", "req-67890")
  
  let consistent_result = CrossServiceConsistencyManager::validate_trace(consistency_manager, consistent_trace)
  assert_true(consistent_result.is_consistent)
  assert_eq(consistent_result.violations.length(), 0)
  
  // 创建不一致的多服务追踪数据
  let inconsistent_trace = MultiServiceTrace::new("inconsistent-trace-002")
  MultiServiceTrace::add_span(inconsistent_trace, "api-gateway", SpanData::new("span-004", 1640995400, 1640995450))
  MultiServiceTrace::add_span(inconsistent_trace, "auth-service", SpanData::new("span-005", 1640996000, 1640996020))  // 时间戳不一致
  MultiServiceTrace::add_span(inconsistent_trace, "user-service", SpanData::new("span-006", 1640995480, 1640995500))
  MultiServiceTrace::add_baggage(inconsistent_trace, "user.id", "user-54321")  // 用户ID不一致
  MultiServiceTrace::add_baggage(inconsistent_trace, "request.id", "req-09876")
  
  let inconsistent_result = CrossServiceConsistencyManager::validate_trace(consistency_manager, inconsistent_trace)
  assert_false(inconsistent_result.is_consistent)
  assert_true(inconsistent_result.violations.length() >= 2)
  
  // 测试一致性修复
  let repair_strategy = AutoRepairStrategy::new(true, false)  // 自动修复时间戳，但不修复业务数据
  let repaired_trace = CrossServiceConsistencyManager::repair_trace(consistency_manager, inconsistent_trace, repair_strategy)
  
  let repaired_result = CrossServiceConsistencyManager::validate_trace(consistency_manager, repaired_trace)
  assert_true(repaired_result.violations.length() < inconsistent_result.violations.length())
}

// 测试10: 遥测数据隐私保护和合规性
test "遥测数据隐私保护和合规性测试" {
  // 创建隐私保护管理器
  let privacy_manager = TelemetryPrivacyManager::new()
  
  // 配置隐私策略
  let pii_masking = PIIMaskingPolicy::new([
    "user.email", "user.phone", "user.address", "credit.card.number"
  ], fn(data) { "*".repeat(data.length() - 4) + data.substring(data.length() - 4) })
  
  let data_retention = DataRetentionPolicy::new(
    90 * 24 * 60 * 60,  // 90天保留期
    ["user.profile", "authentication.log"],  // 敏感数据类型
    "gdpr"  // 合规标准
  )
  
  let consent_management = ConsentManagementPolicy::new([
    "analytics", "performance.monitoring", "error.tracking"
  ], fn(consent_level) { consent_level >= "essential" })
  
  TelemetryPrivacyManager::add_policy(privacy_manager, pii_masking)
  TelemetryPrivacyManager::add_policy(privacy_manager, data_retention)
  TelemetryPrivacyManager::add_policy(privacy_manager, consent_management)
  
  // 创建包含敏感信息的遥测数据
  let sensitive_telemetry = TelemetryData::new()
  TelemetryData::add_attribute(sensitive_telemetry, "user.id", StringValue("user-12345"))
  TelemetryData::add_attribute(sensitive_telemetry, "user.email", StringValue("john.doe@example.com"))
  TelemetryData::add_attribute(sensitive_telemetry, "user.phone", StringValue("+1-555-123-4567"))
  TelemetryData::add_attribute(sensitive_telemetry, "service.name", StringValue("payment.service"))
  TelemetryData::add_attribute(sensitive_telemetry, "request.id", StringValue("req-abc123"))
  
  // 设置用户同意级别
  let user_consent = UserConsent::new("user-12345", ["essential", "analytics"])
  TelemetryPrivacyManager::set_consent(privacy_manager, user_consent)
  
  // 测试隐私保护处理
  let protected_data = TelemetryPrivacyManager::apply_privacy_protection(privacy_manager, sensitive_telemetry)
  
  // 验证PII掩码
  let masked_email = TelemetryData::get_attribute(protected_data, "user.email")
  assert_eq(masked_email, Some(StringValue("********************.com")))
  
  let masked_phone = TelemetryData::get_attribute(protected_data, "user.phone")
  assert_eq(masked_phone, Some(StringValue("************-4567")))
  
  // 验证非敏感数据保持不变
  let service_name = TelemetryData::get_attribute(protected_data, "service.name")
  assert_eq(service_name, Some(StringValue("payment.service")))
  
  // 测试数据保留策略
  let old_data = TelemetryData::with_timestamp(1640995200 - 100 * 24 * 60 * 60)  // 100天前
  TelemetryData::add_attribute(old_data, "user.profile", StringValue("sensitive.profile.data"))
  
  let retention_result = TelemetryPrivacyManager::check_retention_policy(privacy_manager, old_data)
  assert_true(retention_result.should_delete)
  assert_eq(retention_result.reason, "data_retention_expired")
  
  // 测试同意管理
  let analytics_data = TelemetryData::new()
  TelemetryData::add_attribute(analytics_data, "data.type", StringValue("analytics"))
  TelemetryData::add_attribute(analytics_data, "user.id", StringValue("user-12345"))
  
  let consent_result = TelemetryPrivacyManager::check_consent(privacy_manager, analytics_data)
  assert_true(consent_result.allowed)  // 用户同意了analytics
  
  let marketing_data = TelemetryData::new()
  TelemetryData::add_attribute(marketing_data, "data.type", StringValue("marketing"))
  TelemetryData::add_attribute(marketing_data, "user.id", StringValue("user-12345"))
  
  let marketing_consent_result = TelemetryPrivacyManager::check_consent(privacy_manager, marketing_data)
  assert_false(marketing_consent_result.allowed)  // 用户未同意marketing
  
  // 测试合规性报告
  let compliance_report = TelemetryPrivacyManager::generate_compliance_report(privacy_manager, "gdpr")
  assert_true(compliance_report.is_compliant)
  assert_true(compliance_report.policies_applied.contains("pii_masking"))
  assert_true(compliance_report.policies_applied.contains("data_retention"))
  assert_true(compliance_report.policies_applied.contains("consent_management"))
}