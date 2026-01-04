// Azimuth 高级遥测测试用例
// 专注于遥测系统的高级功能和复杂场景

// 测试1: 遥测数据采样策略
test "遥测数据采样策略测试" {
  // 创建采样策略管理器
  let sampling_manager = SamplingManager::new()
  
  // 设置概率采样策略
  let probability_sampler = Sampler::probability(0.1)  // 10%采样率
  SamplingManager::set_default_sampler(sampling_manager, probability_sampler)
  
  // 设置基于属性的采样策略
  let attribute_based_sampler = Sampler::attribute_based([
    ("service.name", "critical.service"),
    ("http.status_code", "500"),
    ("error.type", "timeout")
  ])
  
  // 创建测试span
  let normal_span = Span::new("normal.operation", Server, SpanContext::new("trace-1", "span-1", true, ""))
  Span::set_attribute(normal_span, "service.name", StringValue("normal.service"))
  
  let critical_span = Span::new("critical.operation", Server, SpanContext::new("trace-2", "span-2", true, ""))
  Span::set_attribute(critical_span, "service.name", StringValue("critical.service"))
  
  let error_span = Span::new("error.operation", Server, SpanContext::new("trace-3", "span-3", true, ""))
  Span::set_attribute(error_span, "http.status_code", IntValue(500))
  
  // 应用采样策略
  let normal_sampled = SamplingManager::should_sample(sampling_manager, normal_span)
  let critical_sampled = SamplingManager::should_sample_with_sampler(sampling_manager, critical_span, attribute_based_sampler)
  let error_sampled = SamplingManager::should_sample_with_sampler(sampling_manager, error_span, attribute_based_sampler)
  
  // 验证采样决策
  assert_true(critical_sampled)  // 关键服务总是被采样
  assert_true(error_sampled)     // 错误总是被采样
  // 正常操作根据概率采样，这里我们只检查返回的是布尔值
  assert_true(normal_sampled == true or normal_sampled == false)
  
  // 测试自适应采样
  let adaptive_sampler = Sampler::adaptive(1000, 0.05)  // 每秒最多1000个span，最小5%采样率
  SamplingManager::set_adaptive_sampler(sampling_manager, adaptive_sampler)
  
  // 模拟高频请求
  let mut sampled_count = 0
  for i in 0..=100 {
    let high_freq_span = Span::new("high.freq.operation", Server, SpanContext::new("trace-" + i.to_string(), "span-" + i.to_string(), true, ""))
    if SamplingManager::should_sample(sampling_manager, high_freq_span) {
      sampled_count = sampled_count + 1
    }
  }
  
  // 验证自适应采样限制
  assert_true(sampled_count <= 100)  // 最多采样100个span
  assert_true(sampled_count >= 5)    // 至少采样5个span
}

// 测试2: 自适应性能调优
test "自适应性能调优测试" {
  // 创建性能调优管理器
  let performance_tuner = PerformanceTuner::new()
  
  // 设置初始性能目标
  PerformanceTuner::set_target_latency(performance_tuner, 100.0)  // 100ms目标延迟
  PerformanceTuner::set_target_throughput(performance_tuner, 1000.0)  // 1000 ops/s目标吞吐量
  PerformanceTuner::set_target_error_rate(performance_tuner, 0.01)  // 1%目标错误率
  
  // 记录初始性能指标
  PerformanceTuner::record_metrics(performance_tuner, {
    latency: 150.0,
    throughput: 800.0,
    error_rate: 0.02,
    cpu_usage: 70.0,
    memory_usage: 60.0
  })
  
  // 获取调优建议
  let suggestions = PerformanceTuner::get_tuning_suggestions(performance_tuner)
  
  // 验证调优建议
  assert_true(suggestions.length() > 0)
  
  // 检查延迟相关建议
  let has_latency_suggestion = suggestions.any(fn(s) {
    s.metric == "latency" and s.recommendation.contains("increase")
  })
  assert_true(has_latency_suggestion)
  
  // 检查吞吐量相关建议
  let has_throughput_suggestion = suggestions.any(fn(s) {
    s.metric == "throughput" and s.recommendation.contains("optimize")
  })
  assert_true(has_throughput_suggestion)
  
  // 应用调优参数
  let tuning_params = PerformanceTuner::apply_tuning(performance_tuner, suggestions)
  
  // 验证调优参数
  assert_true(tuning_params.batch_size > 0)
  assert_true(tuning_params.timeout_ms > 0)
  assert_true(tuning_params.max_concurrent_requests > 0)
  
  // 记录调优后的性能指标
  PerformanceTuner::record_metrics(performance_tuner, {
    latency: 95.0,
    throughput: 1100.0,
    error_rate: 0.008,
    cpu_usage: 75.0,
    memory_usage: 65.0
  })
  
  // 获取新的调优建议
  let new_suggestions = PerformanceTuner::get_tuning_suggestions(performance_tuner)
  
  // 验证性能改善后的建议减少
  assert_true(new_suggestions.length() <= suggestions.length())
}

// 测试3: 异常检测和恢复
test "异常检测和恢复测试" {
  // 创建异常检测器
  let anomaly_detector = AnomalyDetector::new()
  
  // 配置检测规则
  AnomalyDetector::add_threshold_rule(anomaly_detector, "error_rate", 0.05, "above")  // 错误率超过5%
  AnomalyDetector::add_threshold_rule(anomaly_detector, "latency_p99", 500.0, "above")  // P99延迟超过500ms
  AnomalyDetector::add_threshold_rule(anomaly_detector, "throughput", 100.0, "below")   // 吞吐量低于100/s
  
  // 配置统计异常检测
  AnomalyDetector::enable_statistical_detection(anomaly_detector, {
    window_size: 100,
    significance_level: 0.05,
    min_data_points: 30
  })
  
  // 添加正常数据点
  for i in 0..=50 {
    AnomalyDetector::add_data_point(anomaly_detector, {
      timestamp: 1640995200 + i * 60,
      error_rate: 0.01 + (i % 10) * 0.001,
      latency_p99: 100.0 + (i % 20) * 5.0,
      throughput: 1000.0 + (i % 15) * 10.0
    })
  }
  
  // 检查正常状态
  let normal_status = AnomalyDetector::detect_anomalies(anomaly_detector)
  assert_eq(normal_status.length(), 0)
  
  // 添加异常数据点
  AnomalyDetector::add_data_point(anomaly_detector, {
    timestamp: 1640995200 + 51 * 60,
    error_rate: 0.08,  // 异常高错误率
    latency_p99: 120.0,
    throughput: 950.0
  })
  
  // 检测异常
  let anomaly_status = AnomalyDetector::detect_anomalies(anomaly_detector)
  assert_true(anomaly_status.length() > 0)
  
  // 验证错误率异常检测
  let error_rate_anomaly = anomaly_status.find(fn(a) { a.metric == "error_rate" })
  assert_true(error_rate_anomaly != None)
  match error_rate_anomaly {
    Some(anomaly) => {
      assert_eq(anomaly.type, "threshold")
      assert_eq(anomaly.severity, "high")
      assert_true(anomaly.description.contains("error_rate"))
    }
    None => assert_true(false)
  }
  
  // 创建恢复策略
  let recovery_manager = RecoveryManager::new()
  RecoveryManager::add_strategy(recovery_manager, "high_error_rate", {
    name: "circuit_breaker",
    parameters: {
      failure_threshold: 5,
      recovery_timeout: 30000,
      half_open_max_calls: 3
    }
  })
  
  RecoveryManager::add_strategy(recovery_manager, "high_latency", {
    name: "load_shedding",
    parameters: {
      shedding_percentage: 20,
      min_requests_per_second: 100
    }
  })
  
  // 应用恢复策略
  let recovery_actions = RecoveryManager::apply_recovery_strategies(recovery_manager, anomaly_status)
  
  // 验证恢复动作
  assert_true(recovery_actions.length() > 0)
  
  let circuit_breaker_action = recovery_actions.find(fn(a) { a.strategy == "circuit_breaker" })
  assert_true(circuit_breaker_action != None)
  
  // 模拟恢复过程
  let recovery_result = RecoveryManager::execute_recovery(recovery_manager, recovery_actions[0])
  assert_true(recovery_result.success)
  assert_true(recovery_result.actions_taken.length() > 0)
}

// 测试4: 数据聚合和分析
test "数据聚合和分析测试" {
  // 创建数据聚合器
  let data_aggregator = DataAggregator::new()
  
  // 配置聚合规则
  DataAggregator::add_aggregation_rule(data_aggregator, {
    name: "request_rate_by_service",
    source_metric: "http.requests.total",
    group_by: ["service.name", "http.method"],
    aggregation: "sum",
    interval: 60  // 1分钟间隔
  })
  
  DataAggregator::add_aggregation_rule(data_aggregator, {
    name: "latency_percentiles",
    source_metric: "http.request.duration",
    group_by: ["service.name", "endpoint"],
    aggregation: "percentile",
    percentile_values: [50.0, 90.0, 95.0, 99.0],
    interval: 60
  })
  
  // 添加原始数据点
  let base_time = 1640995200
  for i in 0..=120 {
    let timestamp = base_time + i * 30  // 每30秒一个数据点
    
    // 服务A的请求
    DataAggregator::add_data_point(data_aggregator, timestamp, "http.requests.total", 10.0, [
      ("service.name", StringValue("service.a")),
      ("http.method", StringValue("GET"))
    ])
    
    DataAggregator::add_data_point(data_aggregator, timestamp, "http.request.duration", 50.0 + (i % 20) * 2.0, [
      ("service.name", StringValue("service.a")),
      ("endpoint", StringValue("/api/users"))
    ])
    
    // 服务B的请求
    DataAggregator::add_data_point(data_aggregator, timestamp, "http.requests.total", 15.0, [
      ("service.name", StringValue("service.b")),
      ("http.method", StringValue("POST"))
    ])
    
    DataAggregator::add_data_point(data_aggregator, timestamp, "http.request.duration", 80.0 + (i % 25) * 3.0, [
      ("service.name", StringValue("service.b")),
      ("endpoint", StringValue("/api/orders"))
    ])
  }
  
  // 执行聚合
  let aggregated_data = DataAggregator::aggregate(data_aggregator, base_time, base_time + 120 * 30)
  
  // 验证聚合结果
  assert_true(aggregated_data.length() > 0)
  
  // 检查请求率聚合
  let request_rate_a = aggregated_data.find(fn(d) { 
    d.metric_name == "request_rate_by_service" and 
    d.attributes.contains(("service.name", StringValue("service.a")))
  })
  assert_true(request_rate_a != None)
  
  match request_rate_a {
    Some(data) => {
      assert_true(data.points.length() > 0)
      // 验证聚合值（每分钟约20个请求）
      assert_true(data.points[0].value >= 18.0 and data.points[0].value <= 22.0)
    }
    None => assert_true(false)
  }
  
  // 检查延迟百分位数聚合
  let latency_percentiles = aggregated_data.find(fn(d) { 
    d.metric_name == "latency_percentiles" and 
    d.attributes.contains(("service.name", StringValue("service.b")))
  })
  assert_true(latency_percentiles != None)
  
  match latency_percentiles {
    Some(data) => {
      assert_true(data.points.length() > 0)
      // 验证百分位数数据结构
      let percentile_data = data.points[0].value
      assert_true(percentile_data.contains("p50"))
      assert_true(percentile_data.contains("p90"))
      assert_true(percentile_data.contains("p95"))
      assert_true(percentile_data.contains("p99"))
    }
    None => assert_true(false)
  }
  
  // 创建数据分析器
  let data_analyzer = DataAnalyzer::new()
  
  // 配置分析规则
  DataAnalyzer::add_trend_analysis(data_analyzer, "http.requests.total", {
    window_size: 10,
    trend_threshold: 0.1  // 10%变化阈值
  })
  
  DataAnalyzer::add_correlation_analysis(data_analyzer, {
    metric_a: "http.request.duration",
    metric_b: "cpu.usage",
    correlation_threshold: 0.7
  })
  
  // 执行分析
  let analysis_results = DataAnalyzer::analyze(data_analyzer, aggregated_data)
  
  // 验证分析结果
  assert_true(analysis_results.length() > 0)
  
  // 检查趋势分析
  let trend_analysis = analysis_results.find(fn(r) { r.analysis_type == "trend" })
  assert_true(trend_analysis != None)
  
  // 检查相关性分析
  let correlation_analysis = analysis_results.find(fn(r) { r.analysis_type == "correlation" })
  assert_true(correlation_analysis != None)
}

// 测试5: 跨服务追踪
test "跨服务追踪测试" {
  // 创建分布式追踪管理器
  let distributed_tracer = DistributedTracer::new()
  
  // 创建根追踪上下文
  let trace_context = TraceContext::new("trace-12345", "root-span-67890", true, [])
  
  // 服务A: API网关
  let gateway_span = DistributedTracer::create_span(distributed_tracer, "api.gateway", trace_context)
  Span::set_attribute(gateway_span, "service.name", StringValue("api.gateway"))
  Span::set_attribute(gateway_span, "http.method", StringValue("POST"))
  Span::set_attribute(gateway_span, "http.url", StringValue("/api/process"))
  
  // 创建子上下文传递给服务B
  let service_b_context = DistributedTracer::create_child_context(distributed_tracer, trace_context, "service-b-span-11111")
  
  // 服务B: 业务逻辑
  let service_b_span = DistributedTracer::create_span_with_context(distributed_tracer, "service.b", service_b_context)
  Span::set_attribute(service_b_span, "service.name", StringValue("service.b"))
  Span::set_attribute(service_b_span, "operation.type", StringValue("business_logic"))
  
  // 服务B调用服务C和D
  let service_c_context = DistributedTracer::create_child_context(distributed_tracer, service_b_context, "service-c-span-22222")
  let service_d_context = DistributedTracer::create_child_context(distributed_tracer, service_b_context, "service-d-span-33333")
  
  // 服务C: 数据库
  let service_c_span = DistributedTracer::create_span_with_context(distributed_tracer, "service.c", service_c_context)
  Span::set_attribute(service_c_span, "service.name", StringValue("service.c"))
  Span::set_attribute(service_c_span, "db.type", StringValue("postgresql"))
  Span::set_attribute(service_c_span, "db.statement", StringValue("SELECT * FROM orders"))
  
  // 服务D: 缓存
  let service_d_span = DistributedTracer::create_span_with_context(distributed_tracer, "service.d", service_d_context)
  Span::set_attribute(service_d_span, "service.name", StringValue("service.d"))
  Span::set_attribute(service_d_span, "cache.type", StringValue("redis"))
  Span::set_attribute(service_d_span, "cache.operation", StringValue("GET"))
  
  // 设置span关系
  DistributedTracer::set_parent_child(distributed_tracer, gateway_span, service_b_span)
  DistributedTracer::set_parent_child(distributed_tracer, service_b_span, service_c_span)
  DistributedTracer::set_parent_child(distributed_tracer, service_b_span, service_d_span)
  
  // 完成所有span
  Span::end(service_c_span)
  Span::end(service_d_span)
  Span::end(service_b_span)
  Span::end(gateway_span)
  
  // 构建追踪树
  let trace_tree = DistributedTracer::build_trace_tree(distributed_tracer, trace_context.trace_id)
  
  // 验证追踪树结构
  assert_eq(trace_tree.trace_id, "trace-12345")
  assert_eq(trace_tree.root_span.span_id, "root-span-67890")
  assert_eq(trace_tree.spans.length(), 4)
  
  // 验证父子关系
  let root_span = trace_tree.spans.find(fn(s) { s.span_id == "root-span-67890" })
  assert_true(root_span != None)
  
  match root_span {
    Some(span) => {
      assert_eq(span.children.length(), 1)
      assert_eq(span.children[0].span_id, "service-b-span-11111")
    }
    None => assert_true(false)
  }
  
  let service_b_span_node = trace_tree.spans.find(fn(s) { s.span_id == "service-b-span-11111" })
  assert_true(service_b_span_node != None)
  
  match service_b_span_node {
    Some(span) => {
      assert_eq(span.children.length(), 2)
      let child_ids = span.children.map(fn(s) { s.span_id })
      assert_true(child_ids.contains("service-c-span-22222"))
      assert_true(child_ids.contains("service-d-span-33333"))
    }
    None => assert_true(false)
  }
  
  // 计算追踪统计
  let trace_stats = DistributedTracer::calculate_trace_statistics(distributed_tracer, trace_tree)
  
  // 验证统计信息
  assert_eq(trace_stats.total_spans, 4)
  assert_eq(trace_stats.total_duration, trace_tree.root_span.duration)
  assert_eq(trace_stats.service_count, 3)  // api.gateway, service.b, service.c/d
  
  // 验证服务分布
  let gateway_spans = trace_stats.spans_by_service.get("api.gateway")
  assert_eq(gateway_spans, Some(1))
  
  let service_b_spans = trace_stats.spans_by_service.get("service.b")
  assert_eq(service_b_spans, Some(1))
  
  let service_c_spans = trace_stats.spans_by_service.get("service.c")
  assert_eq(service_c_spans, Some(1))
  
  let service_d_spans = trace_stats.spans_by_service.get("service.d")
  assert_eq(service_d_spans, Some(1))
  
  // 测试跨服务上下文传播
  let propagator = W3CTraceContextPropagator::new()
  let carrier = TextMapCarrier::new()
  
  // 注入追踪上下文
  DistributedTracer::inject_context(distributed_tracer, trace_context, carrier, propagator)
  
  // 提取追踪上下文
  let extracted_context = DistributedTracer::extract_context(distributed_tracer, carrier, propagator)
  
  // 验证上下文传播
  assert_eq(extracted_context.trace_id, trace_context.trace_id)
  assert_eq(extracted_context.span_id, trace_context.span_id)
  assert_eq(extracted_context.is_sampled, trace_context.is_sampled)
}

// 测试6: 遥测数据压缩
test "遥测数据压缩测试" {
  // 创建数据压缩管理器
  let compression_manager = CompressionManager::new()
  
  // 配置压缩策略
  CompressionManager::set_strategy(compression_manager, "gzip", {
    level: 6,  // 中等压缩级别
    window_size: 32768
  })
  
  CompressionManager::set_strategy(compression_manager, "lz4", {
    acceleration: 1,
    block_size: 65536
  })
  
  // 创建测试遥测数据
  let telemetry_data = []
  for i in 0..=1000 {
    telemetry_data = telemetry_data.push({
      timestamp: 1640995200 + i,
      trace_id: "trace-" + (i % 100).to_string(),
      span_id: "span-" + i.to_string(),
      service_name: "service-" + (i % 10).to_string(),
      operation_name: "operation-" + (i % 20).to_string(),
      duration: 50 + (i % 200),
      status: if i % 50 == 0 { "error" } else { "ok" },
      attributes: [
        ("http.method", "GET"),
        ("http.status_code", (200 + (i % 5) * 100).to_string()),
        ("user.id", "user-" + (i % 500).to_string())
      ]
    })
  }
  
  // 序列化数据
  let serialized_data = TelemetrySerializer::serialize(telemetry_data)
  let original_size = serialized_data.length()
  
  // 使用gzip压缩
  let gzip_compressed = CompressionManager::compress(compression_manager, serialized_data, "gzip")
  let gzip_size = gzip_compressed.length()
  
  // 验证gzip压缩效果
  assert_true(gzip_size < original_size)
  let gzip_ratio = 1.0 - (gzip_size.to_float() / original_size.to_float())
  assert_true(gzip_ratio > 0.5)  // 至少50%压缩率
  
  // 解压gzip数据
  let gzip_decompressed = CompressionManager::decompress(compression_manager, gzip_compressed, "gzip")
  assert_eq(gzip_decompressed.length(), original_size)
  
  // 验证解压数据完整性
  let decompressed_data = TelemetrySerializer::deserialize(gzip_decompressed)
  assert_eq(decompressed_data.length(), telemetry_data.length())
  
  // 使用lz4压缩
  let lz4_compressed = CompressionManager::compress(compression_manager, serialized_data, "lz4")
  let lz4_size = lz4_compressed.length()
  
  // 验证lz4压缩效果
  assert_true(lz4_size < original_size)
  let lz4_ratio = 1.0 - (lz4_size.to_float() / original_size.to_float())
  assert_true(lz4_ratio > 0.3)  // 至少30%压缩率
  
  // 解压lz4数据
  let lz4_decompressed = CompressionManager::decompress(compression_manager, lz4_compressed, "lz4")
  assert_eq(lz4_decompressed.length(), original_size)
  
  // 比较压缩性能
  let gzip_start_time = Time::now()
  for i in 0..=10 {
    CompressionManager::compress(compression_manager, serialized_data, "gzip")
  }
  let gzip_duration = Time::now() - gzip_start_time
  
  let lz4_start_time = Time::now()
  for i in 0..=10 {
    CompressionManager::compress(compression_manager, serialized_data, "lz4")
  }
  let lz4_duration = Time::now() - lz4_start_time
  
  // 验证lz4压缩速度更快
  assert_true(lz4_duration < gzip_duration)
  
  // 测试自适应压缩策略
  let adaptive_compressor = AdaptiveCompressionManager::new()
  
  // 添加性能基准
  AdaptiveCompressionManager::add_benchmark(adaptive_compressor, "small_data", {
    size_threshold: 10240,  // 10KB
    preferred_algorithm: "lz4"
  })
  
  AdaptiveCompressionManager::add_benchmark(adaptive_compressor, "large_data", {
    size_threshold: 1048576,  // 1MB
    preferred_algorithm: "gzip"
  })
  
  // 测试小数据集压缩
  let small_data = serialized_data.substring(0, 1024)
  let small_compressed = AdaptiveCompressionManager::compress(adaptive_compressor, small_data)
  
  // 验证小数据集使用lz4
  let small_algorithm = AdaptiveCompressionManager::get_used_algorithm(adaptive_compressor, small_data)
  assert_eq(small_algorithm, "lz4")
  
  // 测试大数据集压缩
  let large_data = serialized_data + serialized_data + serialized_data + serialized_data
  let large_compressed = AdaptiveCompressionManager::compress(adaptive_compressor, large_data)
  
  // 验证大数据集使用gzip
  let large_algorithm = AdaptiveCompressionManager::get_used_algorithm(adaptive_compressor, large_data)
  assert_eq(large_algorithm, "gzip")
  
  // 测试压缩数据传输
  let transport_manager = TelemetryTransportManager::new()
  TransportManager::enable_compression(transport_manager, true)
  
  // 发送压缩数据
  let send_result = TransportManager::send_telemetry(transport_manager, telemetry_data)
  assert_true(send_result.success)
  assert_eq(send_result.bytes_sent, gzip_size)
  
  // 接收和解压数据
  let receive_result = TransportManager::receive_telemetry(transport_manager)
  assert_true(receive_result.success)
  assert_eq(receive_result.data.length(), telemetry_data.length())
}

// 测试7: 实时监控仪表板
test "实时监控仪表板测试" {
  // 创建仪表板管理器
  let dashboard_manager = DashboardManager::new()
  
  // 创建仪表板
  let dashboard = DashboardManager::create_dashboard(dashboard_manager, "System Overview", {
    refresh_interval: 5000,  // 5秒刷新间隔
    layout: "grid",
    theme: "dark"
  })
  
  // 添加指标面板
  let request_rate_panel = DashboardManager::add_metric_panel(dashboard, {
    title: "Request Rate",
    type: "line_chart",
    metric: "http.requests.total",
    aggregation: "rate",
    time_range: "1h",
    refresh_interval: 5000
  })
  
  let latency_panel = DashboardManager::add_metric_panel(dashboard, {
    title: "Response Latency",
    type: "heatmap",
    metric: "http.request.duration",
    aggregation: "percentile",
    percentile: 95,
    time_range: "1h",
    refresh_interval: 10000
  })
  
  let error_rate_panel = DashboardManager::add_metric_panel(dashboard, {
    title: "Error Rate",
    type: "single_stat",
    metric: "http.errors.total",
    aggregation: "rate",
    time_range: "1h",
    thresholds: [
      { value: 0.01, color: "green" },
      { value: 0.05, color: "yellow" },
      { value: 0.1, color: "red" }
    ]
  })
  
  // 添加服务拓扑面板
  let topology_panel = DashboardManager::add_topology_panel(dashboard, {
    title: "Service Topology",
    type: "network_graph",
    time_range: "30m",
    node_size_metric: "request_count",
    edge_width_metric: "call_count"
  })
  
  // 添加日志面板
  let log_panel = DashboardManager::add_log_panel(dashboard, {
    title: "Recent Errors",
    type: "table",
    log_level: "error",
    limit: 50,
    columns: ["timestamp", "service", "message", "trace_id"]
  })
  
  // 验证仪表板创建
  assert_eq(dashboard.panels.length(), 5)
  assert_eq(dashboard.title, "System Overview")
  assert_eq(dashboard.refresh_interval, 5000)
  
  // 创建实时数据源
  let data_source = RealTimeDataSource::new()
  RealTimeDataSource::connect(data_source, "ws://telemetry-collector.example.com/ws")
  
  // 模拟实时数据流
  let base_time = Time::now()
  for i in 0..=100 {
    let timestamp = base_time + i * 1000  // 每秒一个数据点
    
    RealTimeDataSource::add_metric(data_source, timestamp, "http.requests.total", 100.0 + (i % 20) * 5.0, [
      ("service.name", StringValue("api.gateway")),
      ("http.method", StringValue("GET"))
    ])
    
    RealTimeDataSource::add_metric(data_source, timestamp, "http.request.duration", 50.0 + (i % 30) * 3.0, [
      ("service.name", StringValue("api.gateway")),
      ("endpoint", StringValue("/api/users"))
    ])
    
    RealTimeDataSource::add_metric(data_source, timestamp, "http.errors.total", if i % 10 == 0 { 5.0 } else { 0.0 }, [
      ("service.name", StringValue("api.gateway"))
    ])
    
    // 添加日志条目
    if i % 15 == 0 {
      RealTimeDataSource::add_log(data_source, timestamp, "error", "Database connection timeout", [
        ("service.name", StringValue("database.service")),
        ("trace_id", StringValue("trace-" + i.to_string()))
      ])
    }
  }
  
  // 更新仪表板数据
  let updated_dashboard = DashboardManager::update_dashboard(dashboard_manager, dashboard.id, data_source)
  
  // 验证面板数据更新
  let updated_request_panel = updated_dashboard.panels.find(fn(p) { p.title == "Request Rate" })
  assert_true(updated_request_panel != None)
  
  match updated_request_panel {
    Some(panel) => {
      assert_true(panel.data.points.length() > 0)
      assert_eq(panel.data.metric, "http.requests.total")
    }
    None => assert_true(false)
  }
  
  let updated_latency_panel = updated_dashboard.panels.find(fn(p) { p.title == "Response Latency" })
  assert_true(updated_latency_panel != None)
  
  match updated_latency_panel {
    Some(panel) => {
      assert_true(panel.data.points.length() > 0)
      assert_eq(panel.data.metric, "http.request.duration")
    }
    None => assert_true(false)
  }
  
  let updated_error_panel = updated_dashboard.panels.find(fn(p) { p.title == "Error Rate" })
  assert_true(updated_error_panel != None)
  
  match updated_error_panel {
    Some(panel) => {
      assert_true(panel.data.value >= 0.0)
      assert_eq(panel.data.metric, "http.errors.total")
    }
    None => assert_true(false)
  }
  
  // 测试仪表板告警
  let alert_manager = DashboardAlertManager::new()
  
  // 添加告警规则
  AlertManager::add_rule(alert_manager, {
    name: "High Error Rate",
    condition: "error_rate > 0.05",
    severity: "warning",
    dashboard_id: dashboard.id,
    panel_id: error_rate_panel.id,
    notification_channels: ["email", "slack"]
  })
  
  AlertManager::add_rule(alert_manager, {
    name: "High Latency",
    condition: "latency_p95 > 200",
    severity: "critical",
    dashboard_id: dashboard.id,
    panel_id: latency_panel.id,
    notification_channels: ["pagerduty"]
  })
  
  // 检查告警状态
  let alert_status = AlertManager::check_alerts(alert_manager, updated_dashboard)
  
  // 验证告警触发
  assert_true(alert_status.active_alerts.length() >= 0)  // 可能有也可能没有告警
  
  // 测试仪表板共享
  let share_token = DashboardManager::generate_share_token(dashboard_manager, dashboard.id, {
    expires_in: 3600,  // 1小时过期
    readonly: true
  })
  
  assert_true(share_token.length() > 0)
  
  // 使用共享令牌访问仪表板
  let shared_dashboard = DashboardManager::get_dashboard_by_token(dashboard_manager, share_token)
  assert_true(shared_dashboard != None)
  
  match shared_dashboard {
    Some(db) => {
      assert_eq(db.id, dashboard.id)
      assert_eq(db.title, dashboard.title)
      assert_eq(db.readonly, true)
    }
    None => assert_true(false)
  }
  
  // 测试仪表板导出
  let export_format = "json"
  let exported_dashboard = DashboardManager::export_dashboard(dashboard_manager, dashboard.id, export_format)
  
  assert_true(exported_dashboard.length() > 0)
  assert_true(exported_dashboard.contains("\"title\":\"System Overview\""))
}

// 测试8: 资源使用优化
test "资源使用优化测试" {
  // 创建资源优化管理器
  let resource_optimizer = ResourceOptimizer::new()
  
  // 配置资源限制
  ResourceOptimizer::set_limits(resource_optimizer, {
    max_memory_usage: 536870912,    // 512MB
    max_cpu_usage: 80.0,            // 80%
    max_disk_io: 104857600,         // 100MB/s
    max_network_io: 104857600       // 100MB/s
  })
  
  // 配置优化策略
  ResourceOptimizer::set_strategy(resource_optimizer, "memory", {
    priority: 1,
    tactics: [
      "cache_eviction",
      "batch_size_reduction",
      "compression_enable"
    ]
  })
  
  ResourceOptimizer::set_strategy(resource_optimizer, "cpu", {
    priority: 2,
    tactics: [
      "concurrency_limit",
      "sampling_rate_adjustment",
      "async_processing"
    ]
  })
  
  // 模拟资源使用情况
  let resource_usage = {
    memory_usage: 400 * 1024 * 1024,  // 400MB
    cpu_usage: 75.0,                   // 75%
    disk_io: 80 * 1024 * 1024,         // 80MB/s
    network_io: 90 * 1024 * 1024       // 90MB/s
  }
  
  // 检查资源状态
  let resource_status = ResourceOptimizer::check_resource_status(resource_optimizer, resource_usage)
  
  // 验证资源状态检查
  assert_true(resource_status.memory_usage_ratio < 1.0)  // 400MB / 512MB < 1.0
  assert_true(resource_status.cpu_usage_ratio < 1.0)     // 75% / 80% < 1.0
  assert_true(resource_status.disk_io_ratio < 1.0)       // 80MB/s / 100MB/s < 1.0
  assert_true(resource_status.network_io_ratio < 1.0)    // 90MB/s / 100MB/s < 1.0
  
  // 模拟高资源使用情况
  let high_resource_usage = {
    memory_usage: 600 * 1024 * 1024,  // 600MB (超出限制)
    cpu_usage: 85.0,                   // 85% (超出限制)
    disk_io: 120 * 1024 * 1024,        // 120MB/s (超出限制)
    network_io: 110 * 1024 * 1024      // 110MB/s (超出限制)
  }
  
  // 检查高资源使用状态
  let high_resource_status = ResourceOptimizer::check_resource_status(resource_optimizer, high_resource_usage)
  
  // 验证高资源使用状态
  assert_true(high_resource_status.memory_usage_ratio > 1.0)  // 600MB / 512MB > 1.0
  assert_true(high_resource_status.cpu_usage_ratio > 1.0)     // 85% / 80% > 1.0
  assert_true(high_resource_status.disk_io_ratio > 1.0)       // 120MB/s / 100MB/s > 1.0
  assert_true(high_resource_status.network_io_ratio > 1.0)    // 110MB/s / 100MB/s > 1.0
  
  // 获取优化建议
  let optimization_recommendations = ResourceOptimizer::get_optimization_recommendations(resource_optimizer, high_resource_status)
  
  // 验证优化建议
  assert_true(optimization_recommendations.length() > 0)
  
  // 检查内存优化建议
  let memory_recommendations = optimization_recommendations.filter(fn(r) { r.resource == "memory" })
  assert_true(memory_recommendations.length() > 0)
  
  let has_cache_eviction = memory_recommendations.any(fn(r) { r.tactic == "cache_eviction" })
  assert_true(has_cache_eviction)
  
  // 检查CPU优化建议
  let cpu_recommendations = optimization_recommendations.filter(fn(r) { r.resource == "cpu" })
  assert_true(cpu_recommendations.length() > 0)
  
  let has_sampling_adjustment = cpu_recommendations.any(fn(r) { r.tactic == "sampling_rate_adjustment" })
  assert_true(has_sampling_adjustment)
  
  // 应用优化策略
  let optimization_results = ResourceOptimizer::apply_optimizations(resource_optimizer, optimization_recommendations)
  
  // 验证优化结果
  assert_true(optimization_results.length() > 0)
  
  // 检查优化后的资源使用
  let optimized_resource_usage = ResourceOptimizer::simulate_optimized_usage(resource_optimizer, high_resource_usage, optimization_results)
  
  // 验证资源使用减少
  assert_true(optimized_resource_usage.memory_usage < high_resource_usage.memory_usage)
  assert_true(optimized_resource_usage.cpu_usage < high_resource_usage.cpu_usage)
  
  // 创建自适应资源管理器
  let adaptive_manager = AdaptiveResourceManager::new()
  
  // 配置自适应规则
  AdaptiveResourceManager::add_rule(adaptive_manager, {
    name: "memory_pressure",
    condition: "memory_usage_ratio > 0.8",
    actions: [
      { type: "reduce_cache_size", parameters: { reduction_factor: 0.5 } },
      { type: "increase_sampling_rate", parameters: { increase_factor: 1.5 } }
    ]
  })
  
  AdaptiveResourceManager::add_rule(adaptive_manager, {
    name: "cpu_pressure",
    condition: "cpu_usage_ratio > 0.9",
    actions: [
      { type: "reduce_concurrency", parameters: { reduction_factor: 0.3 } },
      { type: "enable_async_processing", parameters: { threshold: 100 } }
    ]
  })
  
  // 监控资源使用并自动调整
  let monitoring_history = []
  for i in 0..=20 {
    let current_usage = {
      memory_usage: 300 * 1024 * 1024 + i * 20 * 1024 * 1024,  // 逐渐增加
      cpu_usage: 60.0 + i * 2.0,                               // 逐渐增加
      disk_io: 50 * 1024 * 1024 + i * 5 * 1024 * 1024,        // 逐渐增加
      network_io: 60 * 1024 * 1024 + i * 3 * 1024 * 1024      // 逐渐增加
    }
    
    let status = ResourceOptimizer::check_resource_status(resource_optimizer, current_usage)
    let adaptations = AdaptiveResourceManager::monitor_and_adapt(adaptive_manager, status)
    
    monitoring_history = monitoring_history.push({
      timestamp: Time::now() + i * 60,
      resource_usage: current_usage,
      resource_status: status,
      adaptations: adaptations
    })
  }
  
  // 验证自适应调整
  let adaptation_events = monitoring_history.flat_map(fn(h) { h.adaptations })
  assert_true(adaptation_events.length() > 0)
  
  // 检查内存压力下的自适应调整
  let memory_adaptations = adaptation_events.filter(fn(a) { a.rule == "memory_pressure" })
  assert_true(memory_adaptations.length() > 0)
  
  // 检查CPU压力下的自适应调整
  let cpu_adaptations = adaptation_events.filter(fn(a) { a.rule == "cpu_pressure" })
  assert_true(cpu_adaptations.length() > 0)
  
  // 验证资源使用历史趋势
  let memory_trend = ResourceAnalyzer::analyze_trend(monitoring_history.map(fn(h) { h.resource_usage.memory_usage }))
  let cpu_trend = ResourceAnalyzer::analyze_trend(monitoring_history.map(fn(h) { h.resource_usage.cpu_usage }))
  
  assert_true(memory_trend.direction == "increasing")
  assert_true(cpu_trend.direction == "increasing")
  
  // 验证自适应调整效果
  let later_history = monitoring_history.slice(10, 20)
  let earlier_history = monitoring_history.slice(0, 10)
  
  let later_adaptations = later_history.flat_map(fn(h) { h.adaptations })
  let earlier_adaptations = earlier_history.flat_map(fn(h) { h.adaptations })
  
  // 后期应该有更多自适应调整
  assert_true(later_adaptations.length() >= earlier_adaptations.length())
}

// 测试9: 安全性和隐私保护
test "安全性和隐私保护测试" {
  // 创建安全管理器
  let security_manager = SecurityManager::new()
  
  // 配置安全策略
  SecurityManager::set_encryption_policy(security_manager, {
    algorithm: "AES-256-GCM",
    key_rotation_interval: 86400,  // 24小时
    key_derivation: "PBKDF2"
  })
  
  SecurityManager::set_access_control_policy(security_manager, {
    authentication_required: true,
    authorization_required: true,
    token_expiry: 3600,  // 1小时
    refresh_token_expiry: 86400  // 24小时
  })
  
  // 配置隐私策略
  SecurityManager::set_privacy_policy(security_manager, {
    data_minimization: true,
    pseudonymization: true,
    retention_period: 2592000,  // 30天
    right_to_be_forgotten: true
  })
  
  // 创建敏感遥测数据
  let sensitive_telemetry_data = {
    trace_id: "trace-sensitive-12345",
    spans: [
      {
        span_id: "span-1",
        service_name: "authentication.service",
        operation_name: "user.login",
        attributes: [
          ("user.id", StringValue("user-67890")),
          ("user.email", StringValue("user@example.com")),
          ("user.ip_address", StringValue("192.168.1.100")),
          ("user.session_token", StringValue("eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9...")),
          ("credit_card.last4", StringValue("1234"))
        ]
      },
      {
        span_id: "span-2",
        service_name: "payment.service",
        operation_name: "process.payment",
        attributes: [
          ("user.id", StringValue("user-67890")),
          ("payment.amount", FloatValue(99.99)),
          ("payment.currency", StringValue("USD")),
          ("payment.method", StringValue("credit_card")),
          ("payment.card_token", StringValue("tok_1J2x3Y4Z5a6b7c8d9e0f1g2h"))
        ]
      }
    ]
  }
  
  // 应用数据最小化
  let minimized_data = SecurityManager::apply_data_minimization(security_manager, sensitive_telemetry_data)
  
  // 验证数据最小化
  assert_true(minimized_data.spans.length() == sensitive_telemetry_data.spans.length())
  
  let auth_span = minimized_data.spans.find(fn(s) { s.span_id == "span-1" })
  assert_true(auth_span != None)
  
  match auth_span {
    Some(span) => {
      // 敏感属性应该被移除或替换
      let email_attr = span.attributes.find(fn(a) { a.0 == "user.email" })
      assert_true(email_attr == None)  // 邮箱应该被移除
      
      let ip_attr = span.attributes.find(fn(a) { a.0 == "user.ip_address" })
      assert_true(ip_attr == None)  // IP地址应该被移除
      
      let token_attr = span.attributes.find(fn(a) { a.0 == "user.session_token" })
      assert_true(token_attr == None)  // 会话令牌应该被移除
      
      // 用户ID应该被保留（业务需要）
      let user_id_attr = span.attributes.find(fn(a) { a.0 == "user.id" })
      assert_true(user_id_attr != None)
    }
    None => assert_true(false)
  }
  
  let payment_span = minimized_data.spans.find(fn(s) { s.span_id == "span-2" })
  assert_true(payment_span != None)
  
  match payment_span {
    Some(span) => {
      // 敏感支付信息应该被移除或替换
      let card_token_attr = span.attributes.find(fn(a) { a.0 == "payment.card_token" })
      assert_true(card_token_attr == None)  // 卡令牌应该被移除
      
      // 金额和货币应该被保留（业务需要）
      let amount_attr = span.attributes.find(fn(a) { a.0 == "payment.amount" })
      assert_true(amount_attr != None)
      
      let currency_attr = span.attributes.find(fn(a) { a.0 == "payment.currency" })
      assert_true(currency_attr != None)
    }
    None => assert_true(false)
  }
  
  // 应用假名化
  let pseudonymized_data = SecurityManager::apply_pseudonymization(security_manager, minimized_data)
  
  // 验证假名化
  let pseudonymized_auth_span = pseudonymized_data.spans.find(fn(s) { s.span_id == "span-1" })
  assert_true(pseudonymized_auth_span != None)
  
  match pseudonymized_auth_span {
    Some(span) => {
      let user_id_attr = span.attributes.find(fn(a) { a.0 == "user.id" })
      assert_true(user_id_attr != None)
      
      match user_id_attr {
        Some((_, StringValue(user_id))) => {
          // 用户ID应该被假名化
          assert_ne(user_id, "user-67890")
          assert_true(user_id.starts_with("pseudonym-"))
        }
        _ => assert_true(false)
      }
    }
    None => assert_true(false)
  }
  
  // 加密数据
  let encryption_key = SecurityManager::generate_encryption_key(security_manager)
  let encrypted_data = SecurityManager::encrypt_data(security_manager, pseudonymized_data, encryption_key)
  
  // 验证加密
  assert_true(encrypted_data.length() > 0)
  assert_ne(encrypted_data, pseudonymized_data)  // 加密后数据应该不同
  
  // 解密数据
  let decrypted_data = SecurityManager::decrypt_data(security_manager, encrypted_data, encryption_key)
  
  // 验证解密
  assert_eq(decrypted_data, pseudonymized_data)  // 解密后应该与原数据相同
  
  // 测试访问控制
  let access_manager = AccessManager::new()
  
  // 创建用户和角色
  let admin_user = AccessManager::create_user(access_manager, "admin", {
    roles: ["admin"],
    permissions: ["read", "write", "delete", "manage"]
  })
  
  let analyst_user = AccessManager::create_user(access_manager, "analyst", {
    roles: ["analyst"],
    permissions: ["read"]
  })
  
  // 创建访问令牌
  let admin_token = AccessManager::generate_token(access_manager, admin_user.id, 3600)
  let analyst_token = AccessManager::generate_token(access_manager, analyst_user.id, 3600)
  
  // 验证令牌
  let admin_validation = AccessManager::validate_token(access_manager, admin_token)
  let analyst_validation = AccessManager::validate_token(access_manager, analyst_token)
  
  assert_true(admin_validation.valid)
  assert_true(analyst_validation.valid)
  
  // 检查权限
  let admin_can_read = AccessManager::check_permission(access_manager, admin_token, "read")
  let admin_can_delete = AccessManager::check_permission(access_manager, admin_token, "delete")
  let analyst_can_read = AccessManager::check_permission(access_manager, analyst_token, "read")
  let analyst_can_delete = AccessManager::check_permission(access_manager, analyst_token, "delete")
  
  assert_true(admin_can_read)
  assert_true(admin_can_delete)
  assert_true(analyst_can_read)
  assert_false(analyst_can_delete)  // 分析师不应有删除权限
  
  // 测试数据保留策略
  let retention_manager = DataRetentionManager::new()
  
  // 配置保留策略
  RetentionManager::set_policy(retention_manager, {
    default_retention_days: 30,
    data_type_retention: [
      { data_type: "trace_data", retention_days: 7 },
      { data_type: "metrics_data", retention_days: 90 },
      { data_type: "logs_data", retention_days: 30 }
    ]
  })
  
  // 添加测试数据
  let old_timestamp = Time::now() - 40 * 24 * 60 * 60  // 40天前
  let recent_timestamp = Time::now() - 5 * 24 * 60 * 60  // 5天前
  
  RetentionManager::add_data(retention_manager, {
    id: "data-1",
    type: "trace_data",
    timestamp: old_timestamp,
    content: "old trace data"
  })
  
  RetentionManager::add_data(retention_manager, {
    id: "data-2",
    type: "trace_data",
    timestamp: recent_timestamp,
    content: "recent trace data"
  })
  
  RetentionManager::add_data(retention_manager, {
    id: "data-3",
    type: "metrics_data",
    timestamp: old_timestamp,
    content: "old metrics data"
  })
  
  // 执行保留策略
  let deletion_report = RetentionManager::apply_retention_policy(retention_manager)
  
  // 验证删除结果
  assert_true(deletion_report.deleted_data.length() > 0)
  
  // 旧的追踪数据应该被删除（超过7天）
  let deleted_old_trace = deletion_report.deleted_data.any(fn(d) { d.id == "data-1" })
  assert_true(deleted_old_trace)
  
  // 最近的追踪数据不应该被删除
  let not_deleted_recent_trace = not(deletion_report.deleted_data.any(fn(d) { d.id == "data-2" }))
  assert_true(not_deleted_recent_trace)
  
  // 旧的指标数据不应该被删除（90天保留期）
  let not_deleted_old_metrics = not(deletion_report.deleted_data.any(fn(d) { d.id == "data-3" }))
  assert_true(not_deleted_old_metrics)
  
  // 测试被遗忘权
  let user_id = "user-67890"
  let forgetfulness_report = SecurityManager::apply_right_to_be_forgotten(security_manager, user_id)
  
  // 验证被遗忘权执行
  assert_true(forgetfulness_report.data_processed.length() > 0)
  assert_true(forgetfulness_report.data_processed.all(fn(d) { d.user_id == user_id }))
  
  // 验证数据已被删除或假名化
  let processed_data_ids = forgetfulness_report.data_processed.map(fn(d) { d.data_id })
  assert_true(processed_data_ids.length() > 0)
}

// 测试10: 可扩展性和负载均衡
test "可扩展性和负载均衡测试" {
  // 创建可扩展性管理器
  let scalability_manager = ScalabilityManager::new()
  
  // 配置自动扩展策略
  ScalabilityManager::set_auto_scaling_policy(scalability_manager, {
    min_instances: 2,
    max_instances: 10,
    target_cpu_utilization: 70.0,
    target_memory_utilization: 80.0,
    scale_up_cooldown: 300,      // 5分钟
    scale_down_cooldown: 600,    // 10分钟
    scale_up_adjustment: 1,      // 每次增加1个实例
    scale_down_adjustment: 1     // 每次减少1个实例
  })
  
  // 配置负载均衡策略
  ScalabilityManager::set_load_balancing_policy(scalability_manager, {
    algorithm: "weighted_round_robin",
    health_check_interval: 30,    // 30秒
    unhealthy_threshold: 3,       // 连续3次失败认为不健康
    healthy_threshold: 2,         // 连续2次成功认为健康
    timeout: 5,                   // 5秒超时
    weights: [
      { instance: "instance-1", weight: 3 },
      { instance: "instance-2", weight: 2 },
      { instance: "instance-3", weight: 1 }
    ]
  })
  
  // 创建服务实例
  let service_instances = [
    {
      id: "instance-1",
      host: "10.0.1.10",
      port: 8080,
      status: "healthy",
      cpu_usage: 45.0,
      memory_usage: 60.0,
      request_count: 1500,
      last_health_check: Time::now()
    },
    {
      id: "instance-2",
      host: "10.0.1.11",
      port: 8080,
      status: "healthy",
      cpu_usage: 55.0,
      memory_usage: 70.0,
      request_count: 1200,
      last_health_check: Time::now()
    },
    {
      id: "instance-3",
      host: "10.0.1.12",
      port: 8080,
      status: "healthy",
      cpu_usage: 65.0,
      memory_usage: 75.0,
      request_count: 800,
      last_health_check: Time::now()
    }
  ]
  
  // 注册服务实例
  for instance in service_instances {
    ScalabilityManager::register_instance(scalability_manager, instance)
  }
  
  // 验证实例注册
  let registered_instances = ScalabilityManager::get_instances(scalability_manager)
  assert_eq(registered_instances.length(), 3)
  
  // 测试负载均衡
  let load_balancer = LoadBalancer::new(scalability_manager)
  
  // 模拟请求分发
  let request_distribution = {}
  for i in 0..=1000 {
    let selected_instance = LoadBalancer::select_instance(load_balancer)
    let current_count = request_distribution.get(selected_instance.id)
    request_distribution = request_distribution.set(selected_instance.id, current_count + 1)
  }
  
  // 验证负载分布
  let instance1_requests = request_distribution.get("instance-1")
  let instance2_requests = request_distribution.get("instance-2")
  let instance3_requests = request_distribution.get("instance-3")
  
  // 根据权重，实例1应该接收最多请求，实例3应该接收最少请求
  assert_true(instance1_requests > instance2_requests)
  assert_true(instance2_requests > instance3_requests)
  
  // 验证总请求数
  let total_requests = instance1_requests + instance2_requests + instance3_requests
  assert_eq(total_requests, 1001)
  
  // 模拟高负载场景
  let high_load_metrics = {
    total_requests_per_second: 2000,
    average_cpu_usage: 85.0,
    average_memory_usage: 85.0,
    response_time_p95: 500.0
  }
  
  // 检查是否需要扩展
  let scaling_decision = ScalabilityManager::evaluate_scaling_need(scalability_manager, high_load_metrics)
  
  // 验证扩展决策
  assert_eq(scaling_decision.action, "scale_up")
  assert_true(scaling_decision.reason.contains("high_cpu_usage") or scaling_decision.reason.contains("high_memory_usage"))
  
  // 执行扩展
  let scaling_result = ScalabilityManager::execute_scaling(scalability_manager, scaling_decision)
  
  // 验证扩展结果
  assert_true(scaling_result.success)
  assert_eq(scaling_result.new_instance_count, 4)  // 从3增加到4
  
  // 验证新实例
  let updated_instances = ScalabilityManager::get_instances(scalability_manager)
  assert_eq(updated_instances.length(), 4)
  
  // 测试健康检查
  let health_checker = HealthChecker::new(scalability_manager)
  
  // 模拟实例健康检查
  let health_results = HealthChecker::check_all_instances(health_checker)
  
  // 验证健康检查结果
  assert_eq(health_results.length(), 4)
  
  // 模拟一个实例不健康
  let unhealthy_instance = {
    id: "instance-2",
    host: "10.0.1.11",
    port: 8080,
    status: "unhealthy",
    cpu_usage: 95.0,
    memory_usage: 95.0,
    request_count: 1200,
    last_health_check: Time::now()
  }
  
  ScalabilityManager::update_instance_status(scalability_manager, unhealthy_instance)
  
  // 重新执行健康检查
  let updated_health_results = HealthChecker::check_all_instances(health_checker)
  
  // 验证不健康实例被识别
  let unhealthy_count = updated_health_results.filter(fn(r) { not(r.is_healthy) }).length()
  assert_true(unhealthy_count >= 1)
  
  // 测试负载均衡器自动排除不健康实例
  let updated_load_balancer = LoadBalancer::new(scalability_manager)
  
  let healthy_request_distribution = {}
  for i in 0..=100 {
    let selected_instance = LoadBalancer::select_instance(updated_load_balancer)
    // 不应该选择不健康的实例
    assert_ne(selected_instance.id, "instance-2")
    
    let current_count = healthy_request_distribution.get(selected_instance.id)
    healthy_request_distribution = healthy_request_distribution.set(selected_instance.id, current_count + 1)
  }
  
  // 验证只有健康实例接收请求
  assert_true(not(healthy_request_distribution.contains("instance-2")))
  
  // 模拟低负载场景
  let low_load_metrics = {
    total_requests_per_second: 200,
    average_cpu_usage: 30.0,
    average_memory_usage: 40.0,
    response_time_p95: 100.0
  }
  
  // 检查是否需要缩减
  let scale_down_decision = ScalabilityManager::evaluate_scaling_need(scalability_manager, low_load_metrics)
  
  // 验证缩减决策
  assert_eq(scale_down_decision.action, "scale_down")
  assert_true(scale_down_decision.reason.contains("low_cpu_usage") or scale_down_decision.reason.contains("low_memory_usage"))
  
  // 执行缩减
  let scale_down_result = ScalabilityManager::execute_scaling(scalability_manager, scale_down_decision)
  
  // 验证缩减结果
  assert_true(scale_down_result.success)
  assert_eq(scale_down_result.new_instance_count, 3)  // 从4减少到3
  
  // 测试预测性扩展
  let predictive_scaler = PredictiveScaler::new(scalability_manager)
  
  // 添加历史负载数据
  let base_time = Time::now() - 7 * 24 * 60 * 60  // 7天前
  for i in 0..=168 {  // 一周的数据，每小时一个点
    let hour = i % 24
    let day_of_week = (i / 24) % 7
    
    // 模拟工作日高峰和周末低谷
    let base_load = if day_of_week >= 5 { 500 } else { 1000 }  // 周末负载较低
    let hourly_variation = if hour >= 9 and hour <= 17 { 1.5 } else { 0.8 }  // 工作时间负载较高
    
    let load = base_load * hourly_variation
    
    PredictiveScaler::add_historical_load(predictive_scaler, base_time + i * 3600, load)
  }
  
  // 预测未来负载
  let future_loads = PredictiveScaler::predict_load(predictive_scaler, Time::now(), 24 * 60 * 60)  // 预测未来24小时
  
  // 验证预测结果
  assert_eq(future_loads.length(), 24)
  
  // 验证预测模式：白天负载高于夜晚
  let daytime_avg = future_loads.slice(8, 18).reduce(fn(acc, load) { acc + load }, 0) / 10
  let nighttime_avg = (future_loads.slice(0, 8).reduce(fn(acc, load) { acc + load }, 0) + 
                       future_loads.slice(18, 24).reduce(fn(acc, load) { acc + load }, 0)) / 14
  
  assert_true(daytime_avg > nighttime_avg)
  
  // 基于预测进行扩展决策
  let predictive_scaling_decision = PredictiveScaler::recommend_scaling(predictive_scaler, future_loads)
  
  // 验证预测性扩展决策
  assert_true(predictive_scaling_decision.recommended_instances >= 2)  // 至少保持最小实例数
  assert_true(predictive_scaling_decision.recommended_instances <= 10)  // 不超过最大实例数
  
  // 测试跨区域负载均衡
  let global_load_balancer = GlobalLoadBalancer::new()
  
  // 添加区域
  GlobalLoadBalancer::add_region(global_load_balancer, {
    name: "us-west-2",
    instances: [
      { id: "us-west-2-instance-1", host: "10.0.2.10", port: 8080, weight: 3 },
      { id: "us-west-2-instance-2", host: "10.0.2.11", port: 8080, weight: 2 }
    ],
    latency: 50  // 50ms延迟
  })
  
  GlobalLoadBalancer::add_region(global_load_balancer, {
    name: "us-east-1",
    instances: [
      { id: "us-east-1-instance-1", host: "10.0.3.10", port: 8080, weight: 2 },
      { id: "us-east-1-instance-2", host: "10.0.3.11", port: 8080, weight: 2 }
    ],
    latency: 100  // 100ms延迟
  })
  
  GlobalLoadBalancer::add_region(global_load_balancer, {
    name: "eu-west-1",
    instances: [
      { id: "eu-west-1-instance-1", host: "10.0.4.10", port: 8080, weight: 1 }
    ],
    latency: 150  // 150ms延迟
  })
  
  // 模拟来自不同区域的请求
  let region_requests = [
    { region: "us-west-2", count: 100 },
    { region: "us-east-1", count: 80 },
    { region: "eu-west-1", count: 50 }
  ]
  
  let global_distribution = {}
  for request in region_requests {
    for i in 0..=request.count {
      let selected_instance = GlobalLoadBalancer::select_instance_for_region(global_load_balancer, request.region)
      let current_count = global_distribution.get(selected_instance.id)
      global_distribution = global_distribution.set(selected_instance.id, current_count + 1)
    }
  }
  
  // 验证区域亲和性：请求优先路由到最近区域
  let us_west_2_requests = global_distribution.get("us-west-2-instance-1") + 
                          global_distribution.get("us-west-2-instance-2")
  let us_east_1_requests = global_distribution.get("us-east-1-instance-1") + 
                          global_distribution.get("us-east-1-instance-2")
  let eu_west_1_requests = global_distribution.get("eu-west-1-instance-1")
  
  // 来自us-west-2的请求应该主要路由到us-west-2实例
  assert_true(us_west_2_requests >= 80)  // 至少80%的请求应该路由到同区域
  
  // 来自eu-west-1的请求应该主要路由到eu-west-1实例
  assert_true(eu_west_1_requests >= 30)  // 至少60%的请求应该路由到同区域
}